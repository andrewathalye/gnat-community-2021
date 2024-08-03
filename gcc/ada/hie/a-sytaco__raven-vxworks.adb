------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 B o d y                                  --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks/Cert version of this package

with Interfaces.C;
with Ada.Exceptions;
with System.Tasking;

package body Ada.Synchronous_Task_Control with
  SPARK_Mode => Off
is
   use System.OS_Interface;
   use type Interfaces.C.int;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
      St : STATUS;
      pragma Unreferenced (St);
   begin
      --  Need to get the semaphore into the "empty" state.
      --  On return, this task will have made the semaphore
      --  empty (St = OK) or have left it empty.

      St := semTake (S.Sema, NO_WAIT);
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      St : STATUS;
      pragma Unreferenced (St);
   begin
      St := semGive (S.Sema);
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      St : STATUS;
   begin
      --  For this run time, pragma Detect_Blocking is always active, so we
      --  must raise Program_Error if this potentially blocking operation is
      --  called from a protected action (RM H.5 (5/2)).

      if System.Tasking.Self.Common.Protected_Action_Nesting > 0 then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      end if;

      --  Determine whether another task is pending on the suspension
      --  object. Should never be called from an ISR. Therefore semTake can
      --  be called on the mutex

      St := semTake (S.Mutex, NO_WAIT);

      if St = OK then
         --  Wait for suspension object

         St := semTake (S.Sema, WAIT_FOREVER);
         St := semGive (S.Mutex);

      else
         --  Another task is pending on the suspension object

         raise Program_Error;
      end if;
   end Suspend_Until_True;

end Ada.Synchronous_Task_Control;
