------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar/cert version of this package

pragma Restrictions (No_Elaboration_Code);

with System.Secondary_Stack;
with System.Storage_Elements;
with System.Task_Primitives.Operations;

package body System.Tasking is

   use System.Multiprocessors;
   use System.Secondary_Stack;

   package SSL renames System.Soft_Links;

   ------------------------
   -- Local Declarations --
   ------------------------

   Main_Priority : Integer;
   pragma Import (C, Main_Priority, "__gl_main_priority");
   --  Priority associated to the environment task. By default, its
   --  value is undefined, and can be set by using pragma Priority in
   --  the main program. This is a binder generated value (see s-init*.adb)

   Main_CPU : Integer;
   pragma Import (C, Main_CPU, "__gl_main_cpu");
   --  Affinity associated with the environment task. By default, its value is
   --  undefined, and can be set by using pragma CPU in the main program. Its
   --  declaration in this variant is for uniformity with other variants of
   --  s-taskin. This is a binder generated value (see s-init*.adb)

   Environment : aliased Ada_Task_Control_Block (Entry_Num => 0);
   --  ATCB for the environment task

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_TSD return SSL.TSD_Access;

   function  Get_Sec_Stack return SS_Stack_Ptr;

   procedure Set_Sec_Stack (Stack : SS_Stack_Ptr);

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (Task_Entry_Point     : Task_Procedure_Access;
      Task_Arg             : System.Address;
      Base_Priority        : System.Any_Priority;
      Base_CPU             : System.Multiprocessors.CPU_Range;
      Task_Info            : System.Task_Info.Task_Info_Type;
      Stack_Address        : System.Address;
      Stack_Size           : System.Parameters.Size_Type;
      T                    : Task_Id;
      Success              : out Boolean)
   is
   begin
      T.Common.State := Unactivated;

      --  Initialize T.Common.LL

      Task_Primitives.Operations.Initialize_TCB (T, Success);

      if not Success then
         return;
      end if;

      T.Common.Base_Priority            := Base_Priority;
      T.Common.Base_CPU                 := Base_CPU;
      T.Common.Protected_Action_Nesting := 0;
      T.Common.Task_Arg                 := Task_Arg;
      T.Common.Task_Entry_Point         := Task_Entry_Point;
      T.Common.Task_Info                := Task_Info;

      T.Common.Compiler_Data.Pri_Stack_Info.Start_Address :=
        Stack_Address;

      T.Common.Compiler_Data.Pri_Stack_Info.Size :=
        Storage_Elements.Storage_Offset
          (Parameters.Adjust_Storage_Size (Stack_Size));
   end Initialize_ATCB;

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;
   --  Used to prevent multiple calls to Initialize

   procedure Initialize is
      Base_Priority : Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU;

      Success       : Boolean;

      CPU_Not_In_Range : Boolean := False;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Legal values of CPU are the special Unspecified_CPU value which is
      --  inserted by the compiler for tasks without CPU aspect, and those in
      --  the range of CPU_Range but no greater than Number_Of_CPUs. Otherwise
      --  the task is defined to have failed, and it becomes a completed task
      --  (RM D.16(14/3)).

      if Main_CPU /= Unspecified_CPU
        and then (Main_CPU < Integer (System.Multiprocessors.CPU_Range'First)
                    or else
                  Main_CPU > Integer (System.Multiprocessors.Number_Of_CPUs))
      then
         --  Delay the exception until the environment task is initialized

         CPU_Not_In_Range := True;

         --  Use the current CPU as Main_CPU

         Base_CPU := CPU'First; -- Default CPU

      else
         Base_CPU :=
           (if Main_CPU = Unspecified_CPU
              or else CPU_Range (Main_CPU) = Not_A_Specific_CPU
            then CPU'First -- Default CPU
            else CPU (Main_CPU));
      end if;

      --  Set Main_CPU with the selected CPU value
      --  (instead of Unspecified_CPU or Not_A_Specific_CPU)

      Main_CPU := Integer (Base_CPU);

      Base_Priority :=
        (if Main_Priority = Unspecified_Priority
         then Default_Priority
         else Main_Priority);

      Initialize_ATCB
        (null, Null_Address, Base_Priority, Base_CPU,
         Task_Info.Unspecified_Task_Info, Null_Address, 0,
         Environment'Access, Success);
      pragma Assert (Success);

      Task_Primitives.Operations.Initialize (Environment'Access);

      Task_Primitives.Operations.Set_Priority
        (Environment'Access, Base_Priority);

      Environment.Common.State := Runnable;
      Environment.Entry_Call.Self := Environment'Access;

      --  Initialize the secondary stack

      Environment.Common.Compiler_Data.Sec_Stack_Ptr :=
        System.Soft_Links.Get_Sec_Stack_NT;

      SSL.Get_TSD_Addr  := Get_TSD'Access;
      SSL.Get_Sec_Stack := Get_Sec_Stack'Access;
      SSL.Set_Sec_Stack := Set_Sec_Stack'Access;

      if CPU_Not_In_Range then
         raise Tasking_Error with "Main CPU not in range";
      end if;
   end Initialize;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames System.Task_Primitives.Operations.Self;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (T : Task_Id) return System.Parameters.Size_Type is
   begin
      return
         System.Parameters.Size_Type
           (T.Common.Compiler_Data.Pri_Stack_Info.Size);
   end Storage_Size;

   -------------
   -- Get_TSD --
   -------------

   function Get_TSD return SSL.TSD_Access is
      Self_ID : constant Task_Id := Self;
   begin
      return Self_ID.Common.Compiler_Data'Access;
   end Get_TSD;

   ------------------
   -- Get_Sec_Stack--
   ------------------

   function  Get_Sec_Stack return SS_Stack_Ptr is
      Self_ID : constant Task_Id := Self;
   begin
      return Self_ID.Common.Compiler_Data.Sec_Stack_Ptr;
   end Get_Sec_Stack;

   -------------------
   -- Set_Sec_Stack --
   -------------------

   procedure Set_Sec_Stack (Stack : SS_Stack_Ptr) is
      Self_ID : constant Task_Id := Self;
   begin
      Self_ID.Common.Compiler_Data.Sec_Stack_Ptr := Stack;
   end Set_Sec_Stack;

end System.Tasking;
