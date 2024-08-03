------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2021, Free Software Foundation, Inc.         --
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

--  This is the bare board version of this package for RISC-V targets

with Ada.Unchecked_Conversion;

package body System.Traceback is

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
      type Stack_Frame;
      --  Layout of the RISC-V stack frame

      type Stack_Frame_Acc is access Stack_Frame;
      pragma Convention (C, Stack_Frame_Acc);

      type Stack_Frame is record
         Back_Chain : System.Address;
         --  Pointer to previous frame

         Saved_LR   : System.Address;
         --  LR save word
      end record;
      pragma Convention (C, Stack_Frame);

      function Address_To_Stack_Frame_Acc is
         new Ada.Unchecked_Conversion (Source => System.Address,
                                       Target => Stack_Frame_Acc);

      Frame : Stack_Frame_Acc;
      --  Frame being processed

      LR : System.Address;
      --  Link register (return address)

      Last : Integer := Traceback'First - 1;
      --  Index of last traceback entry written to the buffer

      function Builtin_Frame_Address
        (Level : Integer) return System.Address;
      pragma Import
        (Intrinsic, Builtin_Frame_Address, "__builtin_frame_address");

      function FP_To_Stack_Frame_Acc (Addr : System.Address)
                                     return Stack_Frame_Acc;

      pragma No_Inline (FP_To_Stack_Frame_Acc);
      --  We need an actual call to take place to ensure that the return
      --  address is saved in the frame of Call_Chain. Therefore
      --  FP_To_Stack_Frame_Acc is marked as No_Inline.

      ----------------------------
      --  FP_To_Stack_Frame_Acc --
      ----------------------------

      function FP_To_Stack_Frame_Acc (Addr : System.Address)
                                     return Stack_Frame_Acc
      is
      begin
         --  The frame-pointer points above the two words of the frame
         return Address_To_Stack_Frame_Acc (Addr - (Stack_Frame'Size / 8));
      end FP_To_Stack_Frame_Acc;

   begin
      --  We haven't filled any entry so far:

      Len := 0;

      --  Fetch pointer to the current frame:

      Frame := FP_To_Stack_Frame_Acc (Builtin_Frame_Address (0));

      --  Exclude Skip_Frames frames from the traceback. ABI has
      --  System.Null_Address as the back pointer of the shallowest frame in
      --  the stack.

      for J in 1 .. Skip_Frames loop
         Frame := FP_To_Stack_Frame_Acc (Frame.Back_Chain);

         if Frame = null or else Frame.Saved_LR = System.Null_Address then

            --  Something is wrong. Skip_Frames is greater than the number of
            --  frames on the current stack. Do not return a trace.

            return;
         end if;
      end loop;

      while Frame /= null
        and then Last < Traceback'Last
        and then Len < Max_Len
      loop
         LR := Frame.Saved_LR;

         if LR not in Exclude_Min .. Exclude_Max then

            --  Skip specified routines, if any (e.g. Ada.Exceptions)

            Last := Last + 1;
            Len := Len + 1;
            Traceback (Last) := LR;
         end if;

         Frame := FP_To_Stack_Frame_Acc (Frame.Back_Chain);
      end loop;
   end Call_Chain;

end System.Traceback;
