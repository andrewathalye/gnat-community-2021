------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
------------------------------------------------------------------------------

--  This is the VxWorks Ravenscar Cert version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

with System.Init;
with System.OS_Interface;
with System.OS_Versions;
with System.Tasking.Debug;
with System.VxWorks.Ext;
with System.Float_Control;

with Interfaces.C;

package body System.Task_Primitives.Operations is

   use System.Tasking;
   use System.OS_Interface;
   use System.OS_Versions;
   use System.Parameters;
   use type System.VxWorks.Ext.t_id;
   use type Interfaces.Unsigned_16;
   use type Interfaces.C.int;

   ----------------
   -- Local Data --
   ----------------

   CLOCK_REALTIME : constant := 0;
   --  This would usually be obtained from System.OS_Constants, but it is
   --  not used on CERT platforms

   Low_Priority : constant := 255;
   --  VxWorks native (default) lowest scheduling priority

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack limit if
   --  limit checking is used.

   Task_Count : Interfaces.Unsigned_16 := 0;
   --  Count of tasks created so far. Used to create unique part of task name
   --  required by taskOpen for VxWorks Cert 6.x

   Task_Number_Image_Length : constant := 4;
   subtype Task_Number_Image is String (1 .. Task_Number_Image_Length);
   --  Unique part of task name required when using taskOpen instead of
   --  taskSpawn, as necessitated by VxWorks Cert 6.x

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Set (New_Task_Id : Task_Id);
      pragma Inline (Set);
      --  Allocate ATCB and Stack_Limit if needed and set the task ID

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific

   function Created_Task_Count return Task_Number_Image;
   --  Get unique part of task name for use with taskOpen. This is obtained
   --  by incrementing the count of tasks created so far, and then returning
   --  the hexadecimal image of this count.

   function To_VxWorks_Priority
     (Priority : System.OS_Interface.int) return System.OS_Interface.int;
   pragma Inline (To_VxWorks_Priority);
   --  Convert between VxWorks and Ada priority

   function To_Ada_Priority
     (Priority : System.OS_Interface.int) return System.Any_Priority;
   pragma Inline (To_Ada_Priority);
   --  Convert between Ada priority and VxWorks priority

   ------------------------
   -- Created_Task_Count --
   ------------------------

   function Created_Task_Count return Task_Number_Image is
      H : constant array (0 .. 15) of Character := "0123456789ABCDEF";
      --  Table of hex digits

      S : Task_Number_Image;
      N : Integer;

   begin
      Task_Count := Task_Count + 1;

      N := Integer (Task_Count);
      for P in reverse 1 .. S'Last loop
         S (P) := H (N mod 16);
         N := N / 16;
      end loop;

      return S;
   end Created_Task_Count;

   -------------------------
   -- To_VxWorks_Priority --
   -------------------------

   function To_VxWorks_Priority
     (Priority : System.OS_Interface.int) return System.OS_Interface.int
   is
   begin
      return Low_Priority - Priority;
   end To_VxWorks_Priority;

   ---------------------
   -- To_Ada_Priority --
   ---------------------

   function To_Ada_Priority
     (Priority : System.OS_Interface.int) return System.Any_Priority
   is
   begin
      return System.Any_Priority (Low_Priority - Priority);
   end To_Ada_Priority;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Warnings (Off, Reason);

      Result : System.OS_Interface.int;

   begin
      --  Perform a blocking operation to take the CV semaphore

      Result := semTake (Self_ID.Common.LL.CV, WAIT_FOREVER);
      pragma Assert (Result = 0);
   end Sleep;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Time) is
      Current_Time : constant Time := Monotonic_Clock;

      Result : System.OS_Interface.int;
      pragma Unreferenced (Result);

   begin
      if Current_Time < Abs_Time then
         Result := taskDelay (To_Clock_Ticks (Abs_Time - Current_Time));
      else
         Result := taskDelay (0);
      end if;
   end Delay_Until;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Time is
      TS     : aliased timespec;
      Result : int;
   begin
      Result := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return Time (To_Duration (TS));
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Time is
      use Interfaces;

      Ticks_Per_Second : constant Unsigned_64 := Unsigned_64 (sysClkRateGet);
   begin
      return Time (1.0 / (Duration (Ticks_Per_Second)));
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Warnings (Off, Reason);
      Result : System.OS_Interface.int;
   begin
      Result := semGive (T.Common.LL.CV);
      pragma Assert (Result = 0);
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T    : Task_Id;
      Prio : System.Any_Priority)
   is
      Result : System.OS_Interface.int;
   begin
      Result := taskPrioritySet
                  (T.Common.LL.Thread,
                   To_VxWorks_Priority (System.OS_Interface.int (Prio)));
      pragma Assert (Result = 0);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
      Result           : System.OS_Interface.int;
      VxWorks_Priority : aliased System.OS_Interface.int;
   begin
      Result := taskPriorityGet (T.Common.LL.Thread, VxWorks_Priority'Access);
      pragma Assert (Result = 0);
      return To_Ada_Priority (VxWorks_Priority);
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
      Result : System.OS_Interface.int;
      pragma Unreferenced (Result);
   begin

      --  RTP use TLS for the ATCB (aka Self_Id)

      Specific.Set (Self_ID);

      --  Properly initializes the FPU for PPC systems

      System.Float_Control.Reset;

      System.Init.Install_Handler;

      --  Register the task to System.Tasking.Debug

      System.Tasking.Debug.Add_Task_Id (Self_ID);

      --  If stack checking is enabled and limit checking is used, set the
      --  stack limit for this task.

      if Set_Stack_Limit_Hook /= null then
         Set_Stack_Limit_Hook.all;
      end if;
   end Enter_Task;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      Self_ID.Common.LL.CV := semBCreate (SEM_Q_PRIORITY, SEM_EMPTY);
      Succeeded := (if Self_ID.Common.LL.CV = 0 then False else True);
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Base_CPU   : System.Multiprocessors.CPU_Range;
      Succeeded  : out Boolean)
   is
      pragma Unreferenced (Base_CPU);
      Adjusted_Stack_Size : System.OS_Interface.size_t;

      function Get_Task_Options return int;
      pragma Import (C, Get_Task_Options, "__gnat_get_task_options");
      --  Function that returns the options to be set for the task that we
      --  are creating. We fetch the options assigned to the current task,
      --  thus offering some user level control over the options for a task
      --  hierarchy, and force VX_FP_TASK because it is almost always required.

   begin
      --  Add ~1/4 to requested stack size for secondary stack

      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Default_Stack_Size * 5) / 4);
      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Minimum_Stack_Size * 5) / 4);
      else
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Stack_Size * 5) / 4);
      end if;

      pragma Warnings (Off, OS);

      --  Conditional compilation

      if OS = VxWorks_Cert_RTP then

         --  taskSpawn() is not available on VxWorks Cert 6.x for RTPs, so we
         --  have to use taskOpen. Note that taskOpen() is not available in
         --  the Cert kernel unless RTP support is configured in, so we can't
         --  use just one of these APIs for VxWorks 6 Cert.

         declare
            Task_Name_Length : constant := 10;
            Name             : aliased String (1 .. Task_Name_Length);

            Name_Address : System.Address;
            --  Task name we are going to hand down to VxWorks - required for
            --  taskOpen.

            function Get_Object_Options return int;
            pragma Import (C, Get_Object_Options, "__gnat_get_object_options");
            --  These options are needed by taskOpen. They cause the task to be
            --  created unconditionally.

            function taskOpen
              (name : System.Address;
               priority : int;
               options : int;
               mode : int;
               pStackBase : System.Address;
               stackSize : int;
               context : System.Address;
               entryPt : System.Address;
               arg1 : System.Address) return System.VxWorks.Ext.t_id;
            pragma Import (C, taskOpen, "taskOpen");
            --  VxWorks Cert (6.x) does not support taskSpawn for RTPs

         begin
            --  No Ada task names are available for this run-time library, but
            --  taskOpen requires a unique task name, so we construct one.

            Name (1 .. Task_Name_Length) :=
              "tAda_" & Created_Task_Count & ASCII.NUL;

            Name_Address := Name'Address;

            T.Common.LL.Thread := taskOpen
              (Name_Address,
               To_VxWorks_Priority (System.OS_Interface.int (Priority)),
               Get_Task_Options,
               Get_Object_Options,
               System.Null_Address,
               int (Adjusted_Stack_Size),
               System.Null_Address,
               Wrapper,
               To_Address (T));
         end;

         Succeeded := T.Common.LL.Thread /= -1;

      else
         T.Common.LL.Thread := taskSpawn
           (System.Null_Address,
            To_VxWorks_Priority (System.OS_Interface.int (Priority)),
            Get_Task_Options,
            Adjusted_Stack_Size,
            Wrapper,
            To_Address (T));

         Succeeded := T.Common.LL.Thread /= -1;

         --  Don't call Specific.Set here. See comments in s-tpopsp.adb.
         --  See also O723-026 for Stack Checking implications.
      end if;

      pragma Warnings (On, OS);
   end Create_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : System.Tasking.Task_Id) is
   begin

      --  Store the identifier for the environment task

      Operations.Environment_Task := Environment_Task;
      Enter_Task (Environment_Task);
   end Initialize;

   ---------------------
   -- Is_Task_Context --
   ---------------------

   function Is_Task_Context return Boolean is
   begin
      return System.OS_Interface.Interrupt_Context /= 1;
   end Is_Task_Context;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames Specific.Self;

end System.Task_Primitives.Operations;
