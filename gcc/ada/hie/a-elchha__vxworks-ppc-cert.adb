------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2021, Free Software Foundation, Inc.         --
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

--  Default last chance handler for use with the AE653 Level A run-time library
--  and the VxWorks 6 Cert GOS on VxWorks 653 3.x

--  Logs error with health monitor, and dumps exception identity and partial
--  argument string for vxaddr2line for generation of a symbolic stack
--  backtrace.

--  This version cannot reference "adainit" to form the vxaddr2line arguments,
--  as it can be installed in a shared library, possibly with the cert run
--  time. "adainit" is only available in a partition containing an Ada main.

with GNAT.IO;                  use GNAT.IO;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with System.Standard_Library;  use System.Standard_Library;
with System;

procedure Ada.Exceptions.Last_Chance_Handler (Except : Exception_Occurrence) is

   ----------------------
   -- APEX definitions --
   ----------------------

   pragma Warnings (Off);
   type Error_Code_Type is (
      Deadline_Missed,
      Application_Error,
      Numeric_Error,
      Illegal_Request,
      Stack_Overflow,
      Memory_Violation,
      Hardware_Fault,
      Power_Fail);
   pragma Warnings (On);
   pragma Convention (C, Error_Code_Type);
   --  APEX Health Management error codes

   subtype Message_Addr_Type is System.Address;

   subtype Apex_Integer is Integer range -(2 ** 31) .. (2 ** 31) - 1;

   Max_Error_Message_Size : constant := 128;

   subtype Error_Message_Size_Type is Apex_Integer range
      1 .. Max_Error_Message_Size;

   pragma Warnings (Off);
   type Return_Code_Type is (
      No_Error,        --  request valid and operation performed
      No_Action,       --  status of system unaffected by request
      Not_Available,   --  resource required by request unavailable
      Invalid_Param,   --  invalid parameter specified in request
      Invalid_Config,  --  parameter incompatible with configuration
      Invalid_Mode,    --  request incompatible with current mode
      Timed_Out);      --  time-out tied up with request has expired
   pragma Warnings (On);
   pragma Convention (C, Return_Code_Type);
   --  APEX return codes

   procedure Raise_Application_Error
     (Error_Code   : Error_Code_Type;
      Message_Addr : Message_Addr_Type;
      Length       : Error_Message_Size_Type;
      Return_Code  : out Return_Code_Type);
   pragma Import (C, Raise_Application_Error, "RAISE_APPLICATION_ERROR");

   ----------------------
   -- vThreads Imports --
   ----------------------

   procedure Stop (ID : Integer := 0);
   pragma Import (C, Stop, "taskSuspend");
   pragma No_Return (Stop);
   --  Although taskSuspend returns a result, we ignore it,
   --  since in this case (ID = 0 = taskIdSelf) it does not return

   Result : Return_Code_Type;

   Message : String (1 .. Max_Error_Message_Size);

   Message_Length : Error_Message_Size_Type;

begin
   if Except.Id.Name_Length + 25 > Max_Error_Message_Size then
      Message_Length := Max_Error_Message_Size;
   else
      Message_Length := Except.Id.Name_Length + 25;
   end if;

   Message (1 .. 25) := "Unhandled Ada Exception: ";
   Message (26 .. Message_Length - 1) :=
     To_Ptr (Except.Id.Full_Name) (1 .. Message_Length - 26);
   Message (Message_Length) := ASCII.NUL;

   New_Line;
   Put_Line ("In last chance handler");
   Put_Line (Message (1 .. Message_Length - 1));
   New_Line;

   Put_Line ("traceback addresses for vxaddr2line:");

   --  Dump backtrace PC values

   for J in 1 .. Except.Num_Tracebacks loop
      Put (Image_C (Except.Tracebacks (J)));
      Put (" ");
   end loop;

   New_Line;

   Raise_Application_Error
     (Error_Code   => Application_Error,
      Message_Addr => Message_Addr_Type (Message (1)'Address),
      Length       => Message_Length,
      Return_Code  => Result);

   Stop;
end Ada.Exceptions.Last_Chance_Handler;
