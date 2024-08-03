------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar/HI-E version of this package for C targets

with Unchecked_Conversion;

package body Ada.Real_Time with
  SPARK_Mode => Off
is
   function To_Integer is new Unchecked_Conversion (Duration, Integer);

   function Convert_To_Duration is new
     Unchecked_Conversion (Integer, Duration);

   function Monotonic_Clock return Time;
   pragma Import (C, Monotonic_Clock, "rts_monotonic_clock");
   --  Returns time from the board/OS.
   --  This clock implementation is immune to the system's clock changes.

   function RT_Resolution return Integer;
   pragma Import (C, RT_Resolution, "rts_rt_resolution");
   --  Returns resolution of the underlying clock used to implement
   --  Monotonic_Clock.

   function Rounded_Div (L, R : Time_Span) return Time_Span;
   pragma Inline_Always (Rounded_Div);
   --  Return L / R rounded to the nearest integer (to implement ARM D.8 26).

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Left * Time_Span (Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left) * Right;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Left + Time (Right);
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      return Time (Left) + Right;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Integer (Left) + Integer (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      return Left - Time (Right);
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      return Time_Span (Integer (Left) - Integer (Right));
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Integer (Left) - Integer (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
   begin
      return Time_Span (-Integer (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Integer (Left) / Integer (Right);
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Left / Time_Span (Right);
   end "/";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Monotonic_Clock;
   end Clock;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return Rounded_Div (Time_Span (US * RT_Resolution), 10#1#E6);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Rounded_Div (Time_Span (MS * RT_Resolution), 10#1#E3);
   end Milliseconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Milliseconds (M) * Integer'(60_000);
   end Minutes;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Rounded_Div (Time_Span (NS * RT_Resolution), 10#1#E9);
   end Nanoseconds;

   -----------------
   -- Rounded_Div --
   -----------------

   function Rounded_Div (L, R : Time_Span) return Time_Span is
      Left : Time_Span;
   begin
      if L >= 0 then
         Left := L + Time_Span (Long_Integer (R) / 2);
      else
         Left := L - Time_Span (Long_Integer (R) / 2);
      end if;

      return Left / R;
   end Rounded_Div;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Milliseconds (S) * Integer'(1000);
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
      Res : constant Time := Time (RT_Resolution);

   begin
      SC := Seconds_Count (T / Res);

      --  TS will always be non-negative, as required by ARM D.8 (29), because
      --  T is non-negative.

      TS := Time_Span (T) - Time_Span (Time (SC) * Res);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      --  We want to return SC * RT_Resolution + TS. To avoid spurious
      --  overflows in the intermediate result (SC * RT_Resolution) we take
      --  advantage of the different signs in SC and TS, when that is the case.

      --  If the signs of SC and TS are different then we avoid converting SC
      --  to Time (as we do in the else part). The reason for that is that SC
      --  converted to Time may overflow the range of Time, while the addition
      --  of SC plus TS does not overflow (because of their different signs).
      --  The approach is to add and remove the greatest value of time
      --  (greatest absolute value) to both SC and TS. SC and TS have different
      --  signs, so we add the positive constant to the negative value, and the
      --  negative constant to the positive value, to prevent overflows.

      if SC > 0 and then TS < 0 then
         declare
            Closest_Boundary : constant Seconds_Count :=
              (if TS >= 0 then Seconds_Count (Time_Span_Last / RT_Resolution)
               else Seconds_Count (Time_Span_First / RT_Resolution));
            --  Value representing the number of seconds of the Time_Span
            --  boundary closest to TS. The sign of Closest_Boundary is always
            --  different from the sign of SC, hence avoiding overflow in the
            --  expression (SC + Closest_Boundary) * RT_Resolution which
            --  is part of the return statement.

            Dist_To_Boundary : constant Time_Span :=
              TS - Time_Span (Closest_Boundary) * Time_Span (RT_Resolution);
            --  Distance between TS and Closest_Boundary expressed in Time_Span
            --  Both operands in the subtraction have different signs, hence
            --  avoiding overflow.

         begin
            --  Both operands in the inner addition have different signs,
            --  hence avoiding overflow. The Time () conversion and the outer
            --  addition can overflow only if SC + TC is not within Time'Range.

            return Time (SC + Closest_Boundary) * Time (RT_Resolution) +
              Dist_To_Boundary;
         end;

      --  Both operands have the same sign, so we can convert SC into Time
      --  right away; if this conversion overflows then the result of adding SC
      --  and TS would overflow anyway (so we would just be detecting the
      --  overflow a bit earlier).

      else
         return Time (SC) * Time (RT_Resolution) + TS;
      end if;
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
      Result : constant Integer := (Integer (TS) * 10000) / RT_Resolution;
      --  ??? where does this 10000 come from?

   begin
      return Convert_To_Duration (Result);
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      --  Where does this 10000 come from ???
      return Time_Span (To_Integer (D) * RT_Resolution / 10000);
   end To_Time_Span;

end Ada.Real_Time;
