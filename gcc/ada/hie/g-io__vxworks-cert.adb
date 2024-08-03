------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              G N A T . I O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2021, AdaCore                     --
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

--  This version is for the Level A runtime. It eliminates the need
--  for cio.c and related headers.

with Interfaces.C;
with System;

package body GNAT.IO is
   use Interfaces.C;

   Stdin_ID  : constant int := 0;
   Stdout_ID : constant int := 1;
   Stderr_ID : constant int := 2;

   Current_Out : File_Type := Stdout;
   pragma Atomic (Current_Out);
   --  Current output file (modified by Set_Output)

   function Get_File_Descriptor (File : File_Type) return int;
   --  Return the VxWorks global io file descriptor corresponding to File

   -------------------------
   -- Get_File_Descriptor --
   -------------------------

   function Get_File_Descriptor (File : File_Type) return int is
   begin
      case File is
         when Stdout => return Stdout_ID;
         when Stderr => return Stderr_ID;
      end case;
   end Get_File_Descriptor;

   ---------
   -- Get --
   ---------

   procedure Get (X : out Integer) is
      C             : Character;
      Sign          : Integer := +1;
      No_Digit_Seen : Boolean := True;

   begin
      X := 0;
      loop
         Get (C);

         --  Handle initial minus sign

         if No_Digit_Seen
           and then C = '-'
         then
            Sign := -1;

         --  Ignore initial white space

         elsif No_Digit_Seen
           and then (C = ' ' or else C in ASCII.HT .. ASCII.CR)
         then
            null;

         --  Otherwise accumulate digit, we accumulate the negative of the
         --  absolute value, to properly deal with the largest neg number.

         elsif C in '0' .. '9' then
            X := X * 10 - (Character'Pos (C) - Character'Pos ('0'));
            No_Digit_Seen := False;

         else
            exit;
         end if;
      end loop;

      X := (-Sign) * X;
   end Get;

   procedure Get (C : out Character) is
      function read
        (fd       : Interfaces.C.int;
         buffer   : System.Address;
         maxbytes : Interfaces.C.size_t) return Interfaces.C.int;
      pragma Import (C, read, "read");

      Result : Interfaces.C.int;
      Buffer : Interfaces.C.char;

   begin
      Result := read (Stdin_ID, Buffer'Address, 1);
      pragma Assert (Result = 0);
      C := Character (Buffer);
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Item : out String; Last : out Natural) is
      C : Character;

   begin
      for Nstore in Item'Range loop
         Get (C);

         if C = ASCII.LF then
            Last := Nstore - 1;
            return;
         else
            Item (Nstore) := C;
         end if;
      end loop;

      Last := Item'Last;
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Type; Spacing : Positive := 1) is
   begin
      for J in 1 .. Spacing loop
         Put (File, ASCII.LF);
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive := 1) is
   begin
      New_Line (Current_Out, Spacing);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is
   begin
      Put (Current_Out, X);
   end Put;

   procedure Put (File : File_Type; X : Integer) is

      procedure fdprintf
        (File   : Interfaces.C.int;
         Format : String;
         Value  : Interfaces.C.int);
      pragma Import (C_Variadic_2, fdprintf, "fdprintf");

   begin
      fdprintf (Get_File_Descriptor (File), "%d" & ASCII.NUL, int (X));
   end Put;

   procedure Put (C : Character) is
   begin
      Put (Current_Out, C);
   end Put;

   procedure Put (File : File_Type; C : Character) is

      procedure fdprintf
        (File   : Interfaces.C.int;
         Format : String;
         Value  : Character);
      pragma Import (C_Variadic_2, fdprintf, "fdprintf");

   begin
      fdprintf (Get_File_Descriptor (File), "%c" & ASCII.NUL, C);
   end Put;

   procedure Put (S : String) is
   begin
      Put (Current_Out, S);
   end Put;

   procedure Put (File : File_Type; S : String) is
      procedure fdprintf
        (File   : Interfaces.C.int;
         Format : String;
         Value  : String);
      pragma Import (C_Variadic_2, fdprintf, "fdprintf");

      Buffer : String (1 .. S'Length + 1);

   begin
      Buffer (1 .. S'Length) := S;
      Buffer (Buffer'Last) := ASCII.NUL;
      fdprintf (Get_File_Descriptor (File), "%s" & ASCII.NUL, Buffer);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put_Line (Current_Out, S);
   end Put_Line;

   procedure Put_Line (File : File_Type; S : String) is
   begin
      Put (File, S);
      New_Line (File);
   end Put_Line;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      Current_Out := File;
   end Set_Output;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Stdout;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Stderr;
   end Standard_Error;

end GNAT.IO;
