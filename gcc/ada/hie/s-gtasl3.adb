------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . G C C . T I . A S H _ L _ 3                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2013-2021, Free Software Foundation, Inc.          --
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

package body System.GCC.TI.Ash_L_3 is
   use Interfaces;

   -------------
   -- Ashlti3 --
   -------------

   function Ashlti3 (Val : Unsigned_128; Count : Integer) return Unsigned_128
   is
      Hi, Lo : Unsigned_64;
      Carries : Unsigned_64;
      Res : Unsigned_128;
   begin
      Split (Val, Hi, Lo);

      case Count is
         when 128 .. Integer'Last =>
            Res := 0;
         when 64 .. 127 =>
            Res := Merge (Shift_Left (Lo, Count - 64), 0);
         when 1 .. 63 =>
            Carries := Shift_Right (Lo, 64 - Count);
            Lo := Shift_Left (Lo, Count);
            Hi := Shift_Left (Hi, Count) or Carries;
            Res := Merge (Hi, Lo);
         when Integer'First .. 0 =>
            Res := Val;
      end case;

      return Res;
   end Ashlti3;

end System.GCC.TI.Ash_L_3;
