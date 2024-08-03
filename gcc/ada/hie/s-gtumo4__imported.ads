------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . G C C . T I _ U D I V _ M O D _ 4             --
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

--  This is the imported version of __udivmodti4.  The implementation is
--  provided by the system library.  The function is mainly used internally
--  to implement the other div/mod functions.

--  Ada implementation of libgcc: 128-bit Divisions

--  @design @internal
--  This package provides unsigned 128 bit division and remainder.

pragma Restrictions (No_Elaboration_Code);

package System.GCC.TI.Udiv_Mod_4 is
   pragma Pure;

   function Udivmod4 (Num : Unsigned_128;
                      Den : Unsigned_128;
                      Remainder : access Unsigned_128)
                     return Unsigned_128;

private
   pragma Import (C, Udivmod4, "__udivmodti4");

end System.GCC.TI.Udiv_Mod_4;
