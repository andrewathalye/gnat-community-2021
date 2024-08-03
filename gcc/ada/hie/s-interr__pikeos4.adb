------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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

--  This is a version of this package for PikeOS 4.x.

with System.Multiprocessors;
with System.Task_Info;
with System.Tasking;
with System.Tasking.Restricted.Stages;

with System.OS_Interface;     use System.OS_Interface;
with System.Secondary_Stack;  use System.Secondary_Stack;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Interrupts is

   ----------------
   -- Local Data --
   ----------------

   Nbr_Interrupts : constant Natural;
   pragma Import (C, Nbr_Interrupts, "__gnat_nbr_interrupts");
   --  Number of interrupts attached. Set before elaboration in
   --  pikeos-cert-app.c

   Interrupt_Stack_Size : constant := 8 * 1024;
   --  Stack size for an interrupt thread

   Interrupt_Sec_Stack_Size : constant := 1024;
   --  A small secondary stack for the interrupt thread so users are not caught
   --  out in the odd case they try and use the secondary stack in the handler.

   type Handler_Entry is record
      User_Handler : Parameterless_Handler;
      --  The user protected subprogram to be called when an interrupt is
      --  triggered.

      Priority : Interrupt_Priority;
      --  Priority of the protected object

      Id : P4_intid_t;
      --  This handler is for interrupt ID

      ATCB : aliased System.Tasking.Ada_Task_Control_Block (0);
      --  As one task is created per interrupt handler, an ATCB is needed for
      --  the task.

      Stack : Storage_Array (1 .. Interrupt_Stack_Size);
      --  As well as a stack

      Sec_Stack : aliased SS_Stack (Interrupt_Sec_Stack_Size);
      --  and a secondary stack
   end record;

   type Handlers_Table is array (Interrupt_ID) of Handler_Entry;
   --  Type used to represent the procedures used as interrupt handlers.

   Interrupt_Handlers : Handlers_Table;
   --  Table containing user handlers.

   Initialized_Interrupts : array (Interrupt_ID) of Boolean :=
     (others => False);
   --  Set to true when an entry of Interrupt_Handlers has been set. Avoid to
   --  Initialize Interrupt_Handlers array, which is pretty large due to the
   --  stacks.

   Interrupts_Map : array (Interrupt_ID) of P4_intid_t;
   pragma Import (C, Interrupts_Map, "__gnat_interrupts_map");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Install_Handler (Interrupt : Interrupt_ID);
   --  Install the runtime umbrella handler for a hardware interrupt

   procedure Interrupt_Task (Arg : Address);

   procedure Interrupt_Task (Arg : Address)
   is
      Handler : Handler_Entry;
      pragma Import (Ada, Handler);
      for Handler'Address use Arg;

      Res : P4_e_t;
   begin

      --  Attach interrupt

      Res := p4_int_attach_syscall (Handler.Id, P4_INT_MODE_ANY);
      if Res /= P4_E_OK then
         raise Program_Error;
      end if;

      OS_Interface.Set_Interrupt (Handler.Id);

      loop
         --  Wait for interrupt

         Res := OS_Interface.p4_int_wait (P4_TIMEOUT_INFINITE, 0);
         pragma Assert (Res = P4_E_OK);

         pragma Assert (Handler.User_Handler /= null);

         --  Call handler

         --  As exception propagated from a handler that is invoked by an
         --  interrupt must have no effect (ARM C.3 par. 7), interrupt handlers
         --  are wrapped by a null exception handler to avoid exceptions to be
         --  propagated further.

         --  The ravenscar-sfp profile has a No_Exception_Propagation
         --  restriction. Discard compiler warning on the handler.

         pragma Warnings (Off);

         begin
            Handler.User_Handler.all;

         exception

            --  Avoid any further exception propagation

            when others =>
               null;
         end;

         pragma Warnings (On);
      end loop;
   end Interrupt_Task;

   --  Depending on whether exception propagation is supported or not, the
   --  implementation will differ; exceptions can never be propagated through
   --  this procedure (see ARM C.3 par. 7).

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler (Interrupt : Interrupt_ID)
   is
      Handler : Handler_Entry renames Interrupt_Handlers (Interrupt);
      Id : Tasking.Task_Id;
      Chain : Tasking.Activation_Chain;
   begin
      --  Attach the default handler to the specified interrupt. This handler
      --  will in turn call the user handler.

      --  Create a task for the interrupt handler

      Id := Handler.ATCB'Access;
      System.Tasking.Restricted.Stages.Create_Restricted_Task
        (Priority          => Handler.Priority,
         Stack_Address     => Handler.Stack'Address,
         Stack_Size        => Interrupt_Stack_Size,
         Sec_Stack_Address => Handler.Sec_Stack'Access,
         Sec_Stack_Size    => Interrupt_Sec_Stack_Size,
         Task_Info         => System.Task_Info.Unspecified_Task_Info,
         CPU               =>
           Integer (System.Multiprocessors.Not_A_Specific_CPU),
         State             => Interrupt_Task'Access,
         Discriminants     => Handler'Address,
         Elaborated        => null,
         Chain             => Chain,
         Task_Image        => "",
         Created_Task      => Id);

      --  And activate it.

      System.Tasking.Restricted.Stages.Activate_Restricted_Tasks
        (Chain'Unrestricted_Access);
   end Install_Handler;

   ---------------------------------
   -- Install_Restricted_Handlers --
   ---------------------------------

   procedure Install_Restricted_Handlers
     (Prio     : Interrupt_Priority;
      Handlers : Handler_Array)
   is
      use System.Tasking.Restricted.Stages;

   begin
      for H of Handlers loop

         if Natural (H.Interrupt) > Nbr_Interrupts
           or else Initialized_Interrupts (H.Interrupt)
         then
            --  Interrupt already attached. This is not supported.

            raise Program_Error;
         else
            --  Mark the interrupt as attached

            Initialized_Interrupts (H.Interrupt) := True;

            --  Copy the handler in the table that contains the user handlers

            Interrupt_Handlers (H.Interrupt).User_Handler := H.Handler;
            Interrupt_Handlers (H.Interrupt).Priority := Prio;
            Interrupt_Handlers (H.Interrupt).Id :=
              Interrupts_Map (H.Interrupt);

            --  Install the handler now, unless attachment is deferred because
            --  of sequential partition elaboration policy.

            if Partition_Elaboration_Policy /= 'S' then
               Install_Handler (H.Interrupt);
            end if;
         end if;
      end loop;
   end Install_Restricted_Handlers;

   --------------------------------------------
   -- Install_Restricted_Handlers_Sequential --
   --------------------------------------------

   procedure Install_Restricted_Handlers_Sequential is
   begin
      for J in Interrupt_ID loop
         if Initialized_Interrupts (J) then
            Install_Handler (J);
         end if;
      end loop;
   end Install_Restricted_Handlers_Sequential;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   procedure Initialize_Interrupts;
   pragma Import (C, Initialize_Interrupts, "__gnat_init_interrupts");

begin
   --  On PikeOS version < 4.2, this is a NO-OP.
   --  Otherwise, this initializes interrupts as part
   --  of the elaboration of this package.
   Initialize_Interrupts;

end System.Interrupts;
