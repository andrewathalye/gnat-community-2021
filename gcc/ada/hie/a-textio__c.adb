------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
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

--  Version for use with C run time

package body Ada.Text_IO is

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Character) is
      function Putchar (C : Integer) return Integer;
      pragma Import (C, Putchar);

      Ignore : Integer;

   begin
      Ignore := Putchar (Character'Pos (Item));
   end Put;

   procedure Put (Item : String) is
   begin
      for J in Item'Range loop
         Put (Item (J));
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is
      Neg_X : Integer;
      S     : String (1 .. Integer'Width);
      First : Natural := S'Last + 1;
      Val   : Integer;

   begin
      --  Work on negative values to avoid overflows

      Neg_X := (if X < 0 then X else -X);

      loop
         --  Cf RM 4.5.5 Multiplying Operators.  The rem operator will return
         --  negative values for negative values of Neg_X.

         Val := Neg_X rem 10;
         Neg_X := (Neg_X - Val) / 10;
         First := First - 1;
         S (First) := Character'Val (Character'Pos ('0') - Val);
         exit when Neg_X = 0;
      end loop;

      if X < 0 then
         First := First - 1;
         S (First) := '-';
      end if;

      Put (S (First .. S'Last));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

end Ada.Text_IO;
