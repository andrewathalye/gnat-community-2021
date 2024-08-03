------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . T R A C E B A C K _ C O N T R O L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2012-2021, Free Software Foundation, Inc.         --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This is the System.Traceback_Control implementation for PPC/LynxOS-178.

with System.Address_To_Access_Conversions;

package body System.Traceback_Control is

   package Addr is new System.Address_To_Access_Conversions (System.Address);

   ---------------------------
   --  Is_Topframe_Retaddr  --
   ---------------------------

   function Is_Topframe_Retaddr (Retaddr : System.Address) return Boolean is

      --  LynxOS-178 marks call-chain entry points with a return address
      --  pointing to the first instruction of that entry point.

      --  Subprogram'Address yields the address of a descriptor, the first
      --  field of which contains the actual subprogram code-start address,
      --  so we'll have to dereference.

      procedure Process_Entry;
      pragma Import (C, Process_Entry, "__start");

      procedure Thread_Entry;
      pragma Import (C, Thread_Entry, "__runnit");

   begin

      return Retaddr = Addr.To_Pointer (Process_Entry'Address).all
        or else Retaddr = Addr.To_Pointer (Thread_Entry'Address).all

        --  ??? We also have to stop the backtrace prematuraly upon hitting
        --  a return address designating an unexpected region of the address
        --  space, probably originating from a bogus backchain pointer or one
        --  retrieved while trying to walk past a signal handler.

        or else (Retaddr and 16#E0000000#) /= 0;
   end Is_Topframe_Retaddr;

   -----------------------------
   --  Return_Address_Offset  --
   -----------------------------

   function Return_Address_Offset return System.Address is
      Return_Address_Slot_Offset : constant := 8;
      --  Lynx178 obeys the AIX/XCOFF ABI where the return address slot is
      --  located 4 bytes past the 4bytes back-link chain in a frame:

   begin
      return Return_Address_Slot_Offset;
   end Return_Address_Offset;

end System.Traceback_Control;
