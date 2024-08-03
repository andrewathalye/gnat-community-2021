------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
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

--  This is the VxWorks/Cert version of this package

with Interfaces.VxWorks;
with System.OS_Interface; use System.OS_Interface;
with System.Tasking.Restricted.Stages;

package body System.Interrupts is

   -----------------------
   -- Local Subprograms --
   -----------------------

   type Handlers_Array is array (Interrupt_ID) of Parameterless_Handler;
   pragma Suppress_Initialization (Handlers_Array);

   User_Handlers : Handlers_Array := (others => null);
   --  The actual handlers

   procedure Install_Handler (Interrupt : Interrupt_ID);
   --  Install the runtime umbrella handler for a vectored hardware interrupt

   procedure Default_Handler (Interrupt : System.Address);
   --  Default interrupt handler

   ---------------------
   -- Default_Handler --
   ---------------------

   procedure Default_Handler (Interrupt : System.Address) is
   begin
      User_Handlers (Interrupt_ID (Interrupt)).all;
   end Default_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler (Interrupt : Interrupt_ID) is
      Stat : Interfaces.VxWorks.STATUS;
      pragma Unreferenced (Stat);
   begin
      Stat := Interfaces.VxWorks.intConnect
        (Interfaces.VxWorks.Interrupt_Vector (System.Address (Interrupt)),
         Default_Handler'Access,
         System.Address (Interrupt));
   end Install_Handler;

   ---------------------------------
   -- Install_Restricted_Handlers --
   ---------------------------------

   procedure Install_Restricted_Handlers
     (Prio     : Interrupt_Priority;
      Handlers : Handler_Array)
   is
      pragma Unreferenced (Prio);
      use System.Tasking.Restricted.Stages;

   begin
      for J in Handlers'Range loop

         --  Copy the handler in the table that contains the user handlers

         User_Handlers (Handlers (J).Interrupt) := Handlers (J).Handler;

         --  Install the handler now, unless attachment is deferred because of
         --  sequential partition elaboration policy.

         if Partition_Elaboration_Policy /= 'S' then
            Install_Handler (Handlers (J).Interrupt);
         end if;
      end loop;
   end Install_Restricted_Handlers;

   --------------------------------------------
   -- Install_Restricted_Handlers_Sequential --
   --------------------------------------------

   procedure Install_Restricted_Handlers_Sequential is
   begin
      for J in User_Handlers'Range loop
         if User_Handlers (J) /= null then
            Install_Handler (J);
         end if;
      end loop;
   end Install_Restricted_Handlers_Sequential;

end System.Interrupts;
