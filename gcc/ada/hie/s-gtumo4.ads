------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . G C C . T I . U D I V _ M O D _ 4            --
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

--  __udivmodti4 is an internal libgcc function and never called directly
--  so no pragma Export is provided.

--  Ada implementation of libgcc: 128-bit Divisions

--  @design @internal
--  This package provides unsigned 128 bit division and remainder.

pragma Restrictions (No_Elaboration_Code);

with System.GCC.Udiv_Mod_4;
package System.GCC.TI.Udiv_Mod_4 is new
  System.GCC.Udiv_Mod_4 (Unsigned_128, Unsigned_64, Split, Merge,
                         16#ffffffff#, 64, 32) with Pure;
