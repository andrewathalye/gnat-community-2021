------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . G C C . D I V _ M O D _ 3                 --
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

--  Ada generic specification for libgcc: 64 and 128-bit Divisions

--  @design @internal
--  This package provides signed 64 and 128 bit division and remainder.

pragma Restrictions (No_Elaboration_Code);

generic
   type UDW is mod <>;
   type DW is range <>;
   with function Udivmod4 (Num : UDW; Den : UDW; Remainder : access UDW)
     return UDW;
package System.GCC.Div_Mod3 is
   pragma Pure;

   function Divmod3 (Num : DW; Den : DW; Return_Rem : Boolean) return DW;
   --  @internal
   --  Signed division with reminder.
   --  Effects: if Rem /= null then Rem.all = a rem b end if;
   --  Returns: Num / Den

end System.GCC.Div_Mod3;
