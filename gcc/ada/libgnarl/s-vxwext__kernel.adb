------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--            Copyright (C) 2008-2021, Free Software Foundation, Inc.       --
--                                                                          --
-- GNARL is free software;  you can redistribute it  and/or modify it under --
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
------------------------------------------------------------------------------

--  This package provides vxworks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks <= 6.5 kernel version of this package
--  Also works for 6.6 uniprocessor

package body System.VxWorks.Ext is

   ERROR : constant := -1;

   --------------
   -- Int_Lock --
   --------------

   function intLock return int;
   pragma Import (C, intLock, "intLock");

   function Int_Lock return int renames intLock;

   ----------------
   -- Int_Unlock --
   ----------------

   function intUnlock (Old : int) return int;
   pragma Import (C, intUnlock, "intUnlock");

   function Int_Unlock (Old : int) return int renames intUnlock;

   ---------------
   -- semDelete --
   ---------------

   function semDelete (Sem : SEM_ID) return int is
      function Os_Sem_Delete (Sem : SEM_ID) return int;
      pragma Import (C, Os_Sem_Delete, "semDelete");
   begin
      return Os_Sem_Delete (Sem);
   end semDelete;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int is
      pragma Unreferenced (tid, CPU);
   begin
      return ERROR;
   end taskCpuAffinitySet;

   -------------------------
   -- taskMaskAffinitySet --
   -------------------------

   function taskMaskAffinitySet (tid : t_id; CPU_Set : unsigned) return int is
      pragma Unreferenced (tid, CPU_Set);
   begin
      return ERROR;
   end taskMaskAffinitySet;

   ---------------
   -- Task_Cont --
   ---------------

   function Task_Cont (tid : t_id) return int is
      function taskCont (tid : t_id) return int;
      pragma Import (C, taskCont, "taskCont");
   begin
      return taskCont (tid);
   end Task_Cont;

   ---------------
   -- Task_Stop --
   ---------------

   function Task_Stop (tid : t_id) return int is
      function taskStop (tid : t_id) return int;
      pragma Import (C, taskStop, "taskStop");
   begin
      return taskStop (tid);
   end Task_Stop;

end System.VxWorks.Ext;
