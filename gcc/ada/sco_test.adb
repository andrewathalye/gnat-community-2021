------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                             S C O _ T E S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2009-2021, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This utility program is used to test proper operation of the Get_SCOs and
--  Put_SCOs units. To run it, compile any source file with switch -gnateS to
--  get an ALI file file.ALI containing SCO information. Then run this utility
--  using:

--     SCO_Test file.ali

--  This test will read the SCO information from the ALI file, and use Get_SCOs
--  to store this in binary form in the internal tables in SCOs. Then Put_SCO
--  is used to write the information from these tables back into text form.
--  This output is compared with the original SCO information in the ALI file
--  and the two should be identical. If not an error message is output.

with Get_SCOs;
with Put_SCOs;

with Opt;   use Opt;
with Namet; use Namet;
with SCOs;  use SCOs;
with Types; use Types;

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure SCO_Test is
   Infile    : File_Type;
   Name1     : String_Access;
   Outfile_1 : File_Type;
   Name2     : String_Access;
   Outfile_2 : File_Type;
   C         : Character;

   Stop : exception;
   --  Terminate execution

   Diff_Exec   : constant String_Access := Locate_Exec_On_Path ("diff");
   Diff_Result : Integer;

   use ASCII;

begin
   if Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: sco_test FILE.ali");
      raise Stop;
   end if;

   --  Use ALI file name in argument as base for temporary file names, so
   --  that the diff output (if any) contains an indication of that file name.
   --  This also allows several parallel instances of SCO_Test to run in the
   --  same directory without clobbering each other.

   Name1 := new String'(Argument (1) & ".1");
   Name2 := new String'(Argument (1) & ".2");

   Open   (Infile,    In_File,  Argument (1));
   Create (Outfile_1, Out_File, Name1.all);
   Create (Outfile_2, Out_File, Name2.all);

   --  Read input file till we get to first 'C' line

   Process : declare
      function Get_Char (F : File_Type) return Character;
      --  Read one character from specified file

      procedure Put_Char (F : File_Type; C : Character);
      --  Write one character to specified file

      Last_C : Character := ASCII.NUL;
      --  Last character written to Outfile_1, to suppress blank lines

      --------------
      -- Get_Char --
      --------------

      function Get_Char (F : File_Type) return Character is
         Item : Stream_Element_Array (1 .. 1);
         Last : Stream_Element_Offset;

      begin
         Read (F, Item, Last);

         if Last /= 1 then
            return Types.EOF;
         else
            return Character'Val (Item (1));
         end if;
      end Get_Char;

      --------------
      -- Put_Char --
      --------------

      procedure Put_Char (F : File_Type; C : Character) is
         Item : Stream_Element_Array (1 .. 1);
      begin
         if C /= CR and then C /= EOF then
            Item (1) := Character'Pos (C);
            Write (F, Item);
         end if;
      end Put_Char;

      --  Subprograms used by Get_SCO (these also copy the output to Outfile_1
      --  for later comparison with the output generated by Put_SCO).

      function  Getc  return Character;
      function  Nextc return Character;
      procedure Skipc;

      ----------
      -- Getc --
      ----------

      function Getc  return Character is
         C : Character;
      begin
         C := Get_Char (Infile);

         --  Put C to Outfile_1, except when seeing multiple successive LF

         if Last_C /= ASCII.LF or else C /= ASCII.LF then
            Put_Char (Outfile_1, C);
            Last_C := C;
         end if;
         return C;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
         C : Character;
      begin
         C := Get_Char (Infile);

         if C /= EOF then
            Set_Index (Infile, Index (Infile) - 1);
         end if;

         return C;
      end Nextc;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
         C : Character;
         pragma Unreferenced (C);
      begin
         C := Getc;
      end Skipc;

      --  Subprograms used by Put_SCOs, which write information to Outfile_2

      procedure Write_Info_Char (C : Character);
      procedure Write_Info_Initiate (Key : Character);
      procedure Write_Info_Name (Nam : Name_Id);
      procedure Write_Info_Nat (N : Nat);
      procedure Write_Info_Terminate;

      ---------------------
      -- Write_Info_Char --
      ---------------------

      procedure Write_Info_Char (C : Character) is
      begin
         Put_Char (Outfile_2, C);
      end Write_Info_Char;

      -------------------------
      -- Write_Info_Initiate --
      -------------------------

      procedure Write_Info_Initiate (Key : Character) is
      begin
         Write_Info_Char (Key);
      end Write_Info_Initiate;

      ---------------------
      -- Write_Info_Name --
      ---------------------

      procedure Write_Info_Name (Nam : Name_Id) is
      begin
         Get_Name_String (Nam);
         for J in 1 .. Name_Len loop
            Write_Info_Char (Name_Buffer (J));
         end loop;
      end Write_Info_Name;

      --------------------
      -- Write_Info_Nat --
      --------------------

      procedure Write_Info_Nat (N : Nat) is
      begin
         if N > 9 then
            Write_Info_Nat (N / 10);
         end if;

         Write_Info_Char (Character'Val (48 + N mod 10));
      end Write_Info_Nat;

      --------------------------
      -- Write_Info_Terminate --
      --------------------------

      procedure Write_Info_Terminate is
      begin
         Write_Info_Char (LF);
      end Write_Info_Terminate;

      --  Local instantiations of Put_SCOs and Get_SCOs

      procedure Get_SCO_Info is new Get_SCOs;
      procedure Put_SCO_Info is new Put_SCOs;

   --  Start of processing for Process

   begin
      --  Loop to skip till first C line

      loop
         C := Get_Char (Infile);

         if C = EOF then
            raise Stop;

         elsif C = LF or else C = CR then
            loop
               C := Get_Char (Infile);
               exit when C /= LF and then C /= CR;
            end loop;

            exit when C = 'C';
         end if;
      end loop;

      --  Position back to initial C of first C line

      Set_Index (Infile, Index (Infile) - 1);

      --  Read SCOS to internal SCO tables, also copying SCO info to Outfile_1

      SCOs.Initialize;
      Get_SCO_Info;

      --  Write SCOs, including instance table information, from internal SCO
      --  tables to Outfile_2

      Generate_SCO_Instance_Table := True;
      Put_SCO_Info;

      --  Note: when copying the original ALI file to Outfile_1, we remove
      --  blank lines. So, when generating Outfile_2, we must likewise omit
      --  the trailing blank line that normally appears in ALI files (see
      --  comment at end of lib-writ.adb).

      --  Flush to disk

      Close (Outfile_1);
      Close (Outfile_2);

      --  Now Outfile_1 and Outfile_2 should be identical

      Diff_Result :=
        Spawn (Diff_Exec.all,
               Argument_String_To_List
                 ("-u " & Name1.all & " " & Name2.all).all);

      if Diff_Result /= 0 then
         Ada.Text_IO.Put_Line ("diff(1) exit status" & Diff_Result'Img);
      end if;

      OS_Exit (Diff_Result);

   end Process;

exception
   when Stop =>
      null;
end SCO_Test;
