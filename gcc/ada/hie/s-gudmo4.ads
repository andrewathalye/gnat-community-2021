------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . G C C . U D I V _ M O D _ 4               --
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

--  Ada implementation of libgcc: 64 and 128-bit Divisions

--  @design @internal
--  This package provides unsigned 64 and 128 bit division and remainder.

pragma Restrictions (No_Elaboration_Code);
generic
    type UDW is mod <>;
    type UW is mod <>;
    with procedure Split (Val : UDW; Hi : out UW; Lo : out UW);
    with function Merge (Hi : UW; Lo : UW) return  UDW;
    UW_Half_Mask : UW;
    UW_Size : Natural;
    UW_Half_Size : Natural;
package System.GCC.Udiv_Mod_4 is
   pragma Pure;

   function Udivmod4 (Num : UDW; Den : UDW; Remainder : access UDW) return UDW;

end System.GCC.Udiv_Mod_4;
