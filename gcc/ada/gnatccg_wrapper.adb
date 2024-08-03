------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T C C G _ W R A P P E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2021, Free Software Foundation, Inc.         --
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

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

--  Wrapper around <install>/libexec/gnat_ccg/bin/c-xxx to be
--  installed under <install>/bin

function GNATCCG_Wrapper return Integer is

   function Executable_Location return String;
   --  Return the name of the parent directory where the executable is stored
   --  (so if you are running "prefix"/bin/gcc, you would get "prefix").
   --  A special case is done for "bin" directories, which are skipped.
   --  The returned directory always ends up with a directory separator.

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Return True if C is a directory separator

   function Locate_Exec (Exec : String) return String;
   --  Locate Exec from <prefix>/libexec/gnat_ccg/bin. If not found, generate
   --  an error message on stdout and exit with status 1.

   -------------------------
   -- Executable_Location --
   -------------------------

   function Executable_Location return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute or
      --  relative directory where "bin" lies (in the example "C:\usr" or
      --  ".."). If the executable is not a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := GNAT.OS_Lib.Normalize_Pathname
                                  (S, Resolve_Links => True);
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            GNAT.Case_Util.To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         --  If we are not in a bin/ directory

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                     and then
                       not Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return Exec (Exec'First .. Path_Last)
               & GNAT.OS_Lib.Directory_Separator;

         else
            --  Skip bin/, but keep the last directory separator

            return Exec (Exec'First .. Path_Last - 3);
         end if;
      end Get_Install_Dir;

   --  Start of processing for Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Ex  : String_Access   := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Get_Install_Dir (Ex.all);

      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      --  In addition to the default directory_separator allow the '/' to act
      --  as separator.

      return C = Directory_Separator or else C = '/';
   end Is_Directory_Separator;

   Libexec : constant String := Executable_Location & "libexec/gnat_ccg/bin";

   -----------------
   -- Locate_Exec --
   -----------------

   function Locate_Exec (Exec : String) return String is
      Exe : constant String_Access := Get_Target_Executable_Suffix;
      --  Note: the leak on Exe does not matter since this function is called
      --  only once.

      Result : constant String := Libexec & "/" & Exec;

   begin
      if Is_Executable_File (Result & Exe.all) then
         return Result;
      else
         Put_Line (Result & " executable not found, exiting.");
         OS_Exit (1);
      end if;
   end Locate_Exec;

   --  Local variables

   Count    : constant Natural := Argument_Count;
   Path_Val : constant String  := Value ("PATH", "");
   Args     : Argument_List (1 .. Count);
   Status   : Integer;

--  Start of processing for GNATCCG_Wrapper

begin
   --  Add <prefix>/libexec/gnat_ccg/bin in front of the PATH

   Set ("PATH", Libexec & Path_Separator & Path_Val);

   for J in 1 .. Count loop
      Args (J) := new String'(Argument (J));
   end loop;

   Status := Spawn (Locate_Exec (Base_Name (Command_Name, ".exe")), Args);

   for J in Args'Range loop
      Free (Args (J));
   end loop;

   return Status;
end GNATCCG_Wrapper;
