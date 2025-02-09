------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . G C C . T I . D I V _ 3                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2013-2021, Free Software Foundation, Inc.       --
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

--  Ada implementation of libgcc: 128-bit Divisions

--  @design @internal
--  This package provides signed 128 bit division.

pragma Restrictions (No_Elaboration_Code);

package System.GCC.TI.Div_3 is
   pragma Pure;

   function Divti3 (Num : Integer_128; Den : Integer_128) return Integer_128;
   pragma Export (C, Divti3, "__divti3");
   --  Signed division, without checking overflows

end System.GCC.TI.Div_3;
