------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is the Ravenscar/HI-E version of this package for C targets

package Ada.Real_Time with
  SPARK_Mode,
  Abstract_State => (Clock_Time with Synchronous),
  Initializes    => Clock_Time
is
   type Time is private;
   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Unit  : constant := 10#1.0#E-4;

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time with
     Volatile_Function,
     Global => Clock_Time;

   function "+"  (Left : Time;      Right : Time_Span) return Time with
     Global => null;
   function "+"  (Left : Time_Span; Right : Time)      return Time with
     Global => null;
   function "-"  (Left : Time;      Right : Time_Span) return Time with
     Global => null;
   function "-"  (Left : Time;      Right : Time)      return Time_Span with
     Global => null;

   function "<"  (Left, Right : Time) return Boolean with
     Global => null;
   function "<=" (Left, Right : Time) return Boolean with
     Global => null;
   function ">"  (Left, Right : Time) return Boolean with
     Global => null;
   function ">=" (Left, Right : Time) return Boolean with
     Global => null;

   function "+"  (Left, Right : Time_Span)             return Time_Span with
     Global => null;
   function "-"  (Left, Right : Time_Span)             return Time_Span with
     Global => null;
   function "-"  (Right : Time_Span)                   return Time_Span with
     Global => null;
   function "*"  (Left : Time_Span; Right : Integer)   return Time_Span with
     Global => null;
   function "*"  (Left : Integer;   Right : Time_Span) return Time_Span with
     Global => null;
   function "/"  (Left, Right : Time_Span)             return Integer with
     Global => null;
   function "/"  (Left : Time_Span; Right : Integer)   return Time_Span with
     Global => null;

   function "abs" (Right : Time_Span) return Time_Span with
     Global => null;

   function "<"  (Left, Right : Time_Span) return Boolean with
     Global => null;
   function "<=" (Left, Right : Time_Span) return Boolean with
     Global => null;
   function ">"  (Left, Right : Time_Span) return Boolean with
     Global => null;
   function ">=" (Left, Right : Time_Span) return Boolean with
     Global => null;

   function To_Duration  (TS : Time_Span) return Duration with
     Global => null;
   function To_Time_Span (D : Duration)   return Time_Span with
     Global => null;

   function Nanoseconds  (NS : Integer) return Time_Span with
     Global => null;
   function Microseconds (US : Integer) return Time_Span with
     Global => null;
   function Milliseconds (MS : Integer) return Time_Span with
     Global => null;

   function Seconds (S : Integer) return Time_Span with
     Global => null;
   pragma Ada_05 (Seconds);

   function Minutes (M : Integer) return Time_Span with
     Global => null;
   pragma Ada_05 (Minutes);

   --  32 bits is enough for representing Seconds_Count. Time is a 32-bit
   --  unsigned integer, and it then gets divided by the system clock rate.

   type Seconds_Count is range 0 .. 2 ** 31 - 1;

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) with
     Global => null;
   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time with
     Global => null;

private
   pragma SPARK_Mode (Off);

   type Time is mod 2 ** Long_Integer'Size;

   Time_First : constant Time := Time'First;

   Time_Last  : constant Time := Time'Last;

   type Time_Span is new Long_Integer;

   Time_Span_First : constant Time_Span := Time_Span'First;

   Time_Span_Last  : constant Time_Span := Time_Span'Last;

   Time_Span_Zero  : constant Time_Span := 0;

   Time_Span_Unit  : constant Time_Span := 1;

   Tick : constant Time_Span := 1;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

   pragma Inline_Always (Clock);
   pragma Inline_Always ("+");
   pragma Inline_Always ("-");
   pragma Inline_Always ("*");
   pragma Inline_Always ("/");
   pragma Inline_Always (To_Duration);
   pragma Inline_Always (To_Time_Span);
   pragma Inline_Always (Microseconds);
   pragma Inline_Always (Milliseconds);
   pragma Inline_Always (Nanoseconds);
   pragma Inline_Always (Seconds);
   pragma Inline_Always (Minutes);
   pragma Inline_Always (Split);
   pragma Inline_Always (Time_Of);

end Ada.Real_Time;
