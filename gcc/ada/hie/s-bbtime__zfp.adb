------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;
with System.BB.Board_Support;
with System.BB.Protection;
with System.BB.Threads.Queues;
with System.BB.Timing_Events;
with System.BB.CPU_Primitives;
with System.BB.CPU_Specific;

package body System.BB.Time is

   use Board_Support;
   use System.Multiprocessors;

   -----------------------
   -- Local Definitions --
   -----------------------

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown. This type should allow atomic reads and updates.

   Clock_Hi, Clock_Lo : Unsigned_32 := 0;
   pragma Atomic (Clock_Hi);
   pragma Atomic (Clock_Lo);
   --  The current clock split on two 32bit words

   Alarm_Hi, Alarm_Lo : Unsigned_32 := 0;
   --  The current alarm

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt

   -------------------
   -- Alarm_Handler --
   -------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
      pragma Unreferenced (Interrupt);
      Now : constant Time := Clock;

   begin
      Board_Support.Clear_Alarm_Interrupt;

      --  Increment clock

      Clock_Lo := Clock_Lo + 1;
      if Clock_Lo = 0 then
         Clock_Hi := Clock_Hi + 1;
      end if;

      if Clock_Lo = Alarm_Lo and then Clock_Hi = Alarm_Hi then
         --  Note that the code is executed with interruptions disabled, so
         --  there is no need to call Enter_Kernel/Leave_Kernel.

         --  Execute expired events of the current CPU

         Timing_Events.Execute_Expired_Timing_Events (Now);

         --  Wake up our alarms

         Threads.Queues.Wakeup_Expired_Alarms (Now);

         --  Set the timer for the next alarm on this CPU

         Update_Alarm (Get_Next_Timeout (CPU'First));
      end if;

      --  The low-level interrupt handler will call context_switch if necessary

   end Alarm_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      My_Hi, My_Lo : Unsigned_32;
   begin
      --  Get a coherent set of values for the current clock

      loop
         My_Lo := Clock_Lo;
         My_Hi := Clock_Hi;
         exit when My_Lo = Clock_Lo;
      end loop;

      return Time (My_Hi) * (2 ** 32) + Time (My_Lo);
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Now               : Time;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin
      --  First mask interrupts. This is necessary to handle thread queues.

      Protection.Enter_Kernel;

      --  Read the clock once the interrupts are masked to avoid being
      --  interrupted before the alarm is set.

      Now := Clock;

      Self := Threads.Thread_Self;

      --  Test if the alarm time is in the future

      if T > Now then

         --  Extract the thread from the ready queue. When a thread wants to
         --  wait for an alarm it becomes blocked.

         Self.State := Threads.Delayed;

         Threads.Queues.Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if it
         --  was inserted at head then check if Alarm Time is closer than the
         --  next clock interrupt.

         Threads.Queues.Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First then
            Update_Alarm (Get_Next_Timeout (CPU'First));
         end if;

      else
         --  If alarm time is not in the future, the thread must yield the CPU

         Threads.Queues.Yield (Self);
      end if;

      Protection.Leave_Kernel;
   end Delay_Until;

   -----------
   -- Epoch --
   -----------

   function Epoch return Time is
   begin
      --  Initialized to zero at start up

      return 0;
   end Epoch;

   ----------------------
   -- Get_Next_Timeout --
   ----------------------

   function Get_Next_Timeout (CPU_Id : CPU) return Time is
      Alarm_Time : constant Time :=
                     Threads.Queues.Get_Next_Alarm_Time (CPU_Id);
      Event_Time : constant Time := Timing_Events.Get_Next_Timeout (CPU_Id);

   begin
      if Alarm_Time <= Event_Time then
         return Alarm_Time;
      else
         return Event_Time;
      end if;
   end Get_Next_Timeout;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      --  Install alarm handler

      Install_Alarm_Handler (Alarm_Handler'Access);
   end Initialize_Timers;

   ------------------
   -- Update_Alarm --
   ------------------

   procedure Update_Alarm (Alarm : Time) is
   begin
      Alarm_Lo := Unsigned_32 (Alarm mod Unsigned_32'Modulus);
      Alarm_Hi := Unsigned_32 (Alarm / Time (Unsigned_32'Modulus));
   end Update_Alarm;

end System.BB.Time;
