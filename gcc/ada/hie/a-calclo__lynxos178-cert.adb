------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . C A L E N D A R . C L O C K                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2012-2021, AdaCore                      --
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

--  This is the LynxOS-178 Level A cert version of this function

with System;

separate (Ada.Calendar)

-----------
-- Clock --
-----------

function Clock return Time is

   type timeval is array (1 .. 3) of Long_Integer;
   --  The timeval array is sized to contain Long_Long_Integer sec and
   --  Long_Integer usec. If Long_Long_Integer'Size = Long_Integer'Size then it
   --  will be overly large but that will not effect the implementation since
   --  it is not accessed directly.

   function gettimeofday
     (Tv : access timeval;
      Tz : System.Address := System.Null_Address) return Integer;
   pragma Import (C, gettimeofday, "gettimeofday");

   procedure timeval_to_duration
     (T    : not null access timeval;
      sec  : not null access Long_Long_Integer;
      usec : not null access Long_Integer);
   pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

   Elapsed_Seconds : Duration;
   Elapsed_Days    : Time;
   Micro           : constant := 10**6;
   Result          : Integer;
   sec             : aliased Long_Long_Integer;
   TV              : aliased timeval;
   usec            : aliased Long_Integer;
   pragma Unreferenced (Result);

begin
   --  The return codes for gettimeofday are as follows (from man pages):
   --    EPERM  settimeofday is called by someone other than the superuser
   --    EINVAL Timezone (or something else) is invalid
   --    EFAULT One of tv or tz pointed outside accessible address space

   --  None of these codes signal a potential clock skew, hence the return
   --  value is never checked.

   Result := gettimeofday (TV'Access, System.Null_Address);
   timeval_to_duration (TV'Access, sec'Access, usec'Access);
   Elapsed_Seconds := Duration (sec) + Duration (usec) / Micro;
   Elapsed_Days := Elapsed_Seconds / Secs_Per_Day;
   return Radix_Time + Elapsed_Days;
end Clock;
