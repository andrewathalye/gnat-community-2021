------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           S Y S T E M . I N I T                          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2003-2021, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Level A cert version for VxWorks 6 Cert, VxWorks 653, and MILS

--  This file should be kept synchronized with init.c and s-osvers-*.ads, and
--  s-init__*.adb. All these files implement the required functionality for
--  different targets.

with Interfaces.C;
with System.Secondary_Stack;
with System.Soft_Links;
with System.OS_Versions;

package body System.Init is

   use Interfaces.C;
   use System.Secondary_Stack;
   use System.Soft_Links;
   use System.OS_Versions;

   Runtime_Initialized : Boolean := False;
   --  Guard to ensure the runtime is initialized only once

   Stack_Limit_Hook : System.Address;
   pragma Export (C, Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Used when stack limit checking is the stack overflow checking method

   -----------------------------
   -- Binder Generated Values --
   -----------------------------

   Gl_Main_Priority : Integer := -1;
   pragma Export (C, Gl_Main_Priority, "__gl_main_priority");

   Gl_Main_CPU : Integer := -1;
   pragma Export (C, Gl_Main_CPU, "__gl_main_cpu");

   ------------------------
   -- Signal Definitions --
   ------------------------

   NSIG : constant := 32;
   --  Number of signals on the target OS

   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

   SIGILL  : constant :=  4; --  illegal instruction (not reset)
   SIGFPE  : constant :=  8; --  floating point exception
   SIGBUS  : constant := 10; --  bus error
   SIGSEGV : constant := 11; --  segmentation violation

   type sigset_t is
     mod 2 ** (case OS is
                 when VxWorks_Cert     |
                      VxWorks_Cert_RTP => Long_Long_Integer'Size,
                 when others           => Long_Integer'Size);

   SIG_SETMASK : constant := 3;
   SA_ONSTACK   : constant := 16#0004#;

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   function sigdelset (set : access sigset_t; sig : Signal) return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, "sigprocmask");

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Clear_Exception_Count;
   pragma Import (C, Clear_Exception_Count, "__gnat_clear_exception_count");
   --  Clear count of nested hardware exceptions when handling signal, as
   --  required for vThreads.

   procedure GNAT_Error_Handler (Sig : Signal);
   pragma No_Return (GNAT_Error_Handler);
   --  Common procedure that is executed when a SIGFPE, SIGILL, SIGSEGV, or
   --  SIGBUS is captured.

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Sig : Signal) is
      Mask   : aliased sigset_t;
      Result : int;
      pragma Unreferenced (Result);

   begin
      --  VxWorks will always mask out the signal during the signal handler and
      --  will reenable it on a longjmp. GNAT does not generate a longjmp to
      --  return from a signal handler so the signal will still be masked
      --  unless we unmask it.

      Result := pthread_sigmask (SIG_SETMASK, null, Mask'Unchecked_Access);
      Result := sigdelset (Mask'Unchecked_Access, Sig);
      Result := pthread_sigmask (SIG_SETMASK, Mask'Unchecked_Access, null);
      Clear_Exception_Count;

      --  The VxWorks MILS VxWorks 5 Guest OS and Vx6Cert do not support stack
      --  probing via guard pages, so the use of signals differs from the other
      --  VxWorks variants. Warnings are off because of constant condition
      --  values.

      pragma Warnings (Off);
      case Sig is
         when SIGFPE =>
            raise Constraint_Error with "SIGFPE";

         when SIGILL =>
            if OS = VxWorks_MILS or else OS = VxWorks_Cert then
               raise Storage_Error with "possible stack overflow, SIGILL";
            elsif OS = VxWorks_653 then
               raise Constraint_Error with "Floating point overflow or SIGILL";
            else
               raise Constraint_Error with "SIGILL";
            end if;

         when SIGSEGV =>
            raise Storage_Error with "SIGSEGV";

         when SIGBUS =>
            if OS = VxWorks_MILS or else OS = VxWorks_Cert then
               raise Program_Error with "SIGBUS";
            else
               raise Storage_Error with "possible stack overflow, SIGBUS";
            end if;

         when others =>
            raise Program_Error with "unexpected signal";
      end case;
      pragma Warnings (On);
   end GNAT_Error_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
      Mask          : aliased sigset_t;
      Signal_Action : aliased struct_sigaction;

      Result : Interfaces.C.int;
      pragma Unreferenced (Result);

   begin
      --  Set up signal handler to map synchronous signals to appropriate
      --  exceptions. Make sure that the handler isn't interrupted by
      --  another signal that might cause a scheduling event.

      Signal_Action.sa_handler := GNAT_Error_Handler'Address;
      Signal_Action.sa_flags := SA_ONSTACK;
      Result := sigemptyset (Mask'Unchecked_Access);
      Signal_Action.sa_mask := Mask;

      Result := sigaction
        (Signal (SIGFPE), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGILL), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGSEGV), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGBUS), Signal_Action'Unchecked_Access, null);
   end Install_Handler;

   ------------------------
   -- Runtime_Initialize --
   ------------------------

   procedure Runtime_Initialize (Install_Handler : Integer) is
      pragma Unreferenced (Install_Handler);

      Sec_Stack : SS_Stack_Ptr := null;
   begin
      if Runtime_Initialized then
         return;
      end if;

      Init.Install_Handler;
      SS_Init (Sec_Stack);
      Set_Sec_Stack_NT (Sec_Stack);

      Runtime_Initialized := True;
   end Runtime_Initialize;

   ----------------------
   -- Runtime_Finalize --
   ----------------------

   procedure Runtime_Finalize is
   begin
      null;
   end Runtime_Finalize;
end System.Init;
