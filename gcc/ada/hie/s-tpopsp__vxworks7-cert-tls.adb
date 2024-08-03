------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 1998-2021, AdaCore                    --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a VxWorks version of this package using Thread_Local_Storage
--  support for use with ravenscar-cert and ravenscar-cert-rtp. It assumes
--  VxWorks7r2 Cert.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB : aliased Task_Id := null;
   pragma Thread_Local_Storage (ATCB);
   --  Ada Task_Id associated with a thread

   ---------
   -- Set --
   ---------

   procedure Set (New_Task_Id : Task_Id) is
   begin

      --  Note that this TLS version of Set should only be called from
      --  Enter_Task.  In some (now defunct) versions of VxWorks it was
      --  called from both Create_Task, Initialize and Enter_Task, but
      --  this only worked by accident.

      --  The offending scenario is Set is called from Initialize (either
      --  directly or via Enter_Task) for the environment task.  Then Set
      --  is called from Create_Task for the tasks, but the context is still
      --  the environment task, then Set is called again from Enter_Task.
      --  However the ATCB for the environment task gets repeatedly
      --  overwritten  by the call in Create_Task and ends up set to the
      --  Task_Id of the last task created.  Havoc ensues because critical
      --  data stored in the TSD area for the environment task is actually
      --  data related to the last task created.

      pragma Assert (ATCB = null);
      ATCB := New_Task_Id;
   end Set;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      return ATCB;
   end Self;

end Specific;
