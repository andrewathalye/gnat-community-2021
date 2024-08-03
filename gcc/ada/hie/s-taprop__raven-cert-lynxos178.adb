------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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

--  This is the LynxOS-178 Ravenscar Cert version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System.Init;
with System.OS_Interface;
with System.Task_Info;
with System.Tasking.Debug;
with System.Float_Control;

package body System.Task_Primitives.Operations is

   use System.Tasking;
   use System.OS_Interface;
   use System.Parameters;
   use type Interfaces.C.int;

   ----------------
   -- Local Data --
   ----------------

   CLOCK_REALTIME : constant := 0;
   --  This would usually be obtained from System.OS_Constants, but that
   --  package is not used on cert platforms.

   Max_System_Delay : constant Time := 2147483.0;
   --  This would ususally be obtained from System.OS_Primiitives, but that
   --  package is not used on cert platforms.

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack limit if
   --  limit checking is used.

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Initialize;
      pragma Inline (Initialize);
      --  Initialize the thread specific data

      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : Task_Id;
      Reason  : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);

      Result : Interfaces.C.int;

   begin
      Result :=
        pthread_mutex_lock (mutex => Self_ID.Common.LL.L'Access);
      pragma Assert (Result = 0);

      --  If Wakeup was already called before Self_ID.Common.LL.L was locked,
      --  we simply keep running (don't call pthread_cond_wait)

      if not Self_ID.Common.Wakeup_Signaled then
         Result :=
           pthread_cond_wait
             (cond  => Self_ID.Common.LL.CV'Access,
              mutex => (Self_ID.Common.LL.L'Access));

         --  EINTR is not considered a failure

         pragma Assert (Result = 0 or else Result = EINTR);
      end if;

      Self_ID.Common.Wakeup_Signaled := False;

      Result :=
        pthread_mutex_unlock (mutex => Self_ID.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Sleep;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Time) is
      Self_ID    : constant Task_Id := Specific.Self;
      Request    : aliased timespec;
      Result     : int;
      P_Abs_Time : Time;
      Check_Time : Time := Monotonic_Clock;
      Exit_Outer : Boolean := False;

   begin
      Result :=
        pthread_mutex_lock (mutex => Self_ID.Common.LL.L'Access);
      pragma Assert (Result = 0);

      Outer : loop
         if Abs_Time > Check_Time + Max_System_Delay then
            P_Abs_Time := Check_Time + Max_System_Delay;
         else
            P_Abs_Time := Abs_Time;
            Exit_Outer := True;
         end if;

         Request  := To_Timespec (P_Abs_Time);

         Inner : loop
            Result :=
              pthread_cond_timedwait
                (cond    => Self_ID.Common.LL.CV'Access,
                 mutex   => Self_ID.Common.LL.L'Access,
                 abstime => Request'Access);

            case Result is
               when 0 =>

                  --  Spurious wakeup due to interrupt. Go around the loop and
                  --  wait again. The delay amount is absolute, so we don't
                  --  need to read the clock or do any time calculations.
                  --  Note that the mutex has already been reacquired in this
                  --  case.

                  null;

               when ETIMEDOUT =>

                  --  pthread_cond_timedwait timed out, which is what we want

                  exit Outer when Exit_Outer;
                  Check_Time := Monotonic_Clock;
                  exit Inner;

               when others =>

                  --  Should not be any other possibilities

                  pragma Assert (Standard.False);
            end case;
         end loop Inner;
      end loop Outer;

      pragma Assert (Monotonic_Clock >= Abs_Time);

      Result :=
        pthread_mutex_unlock (mutex => Self_ID.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Delay_Until;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;

   begin
      --  Result := clock_gettime
      --    (clock_id => OSC.CLOCK_RT_Ada, tp => TS'Unchecked_Access);
      --  ??? Check value later

      Result :=
        clock_gettime (clock_id => CLOCK_REALTIME, tp => TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return To_Duration (TS);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;
   begin
      Result := clock_getres (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return To_Duration (TS);
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_lock (mutex => T.Common.LL.L'Access);
      pragma Assert (Result = 0);

      --  Wakeup_Signaled avoids a potential race condition, in case this is
      --  called just before T calls Sleep.

      pragma Assert (not T.Common.Wakeup_Signaled);
      T.Common.Wakeup_Signaled := True;

      Result := pthread_cond_signal (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);

      Result := pthread_mutex_unlock (mutex => T.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T    : Task_Id;
      Prio : System.Any_Priority)
   is
      Result : Interfaces.C.int;
      Param  : aliased struct_sched_param;

   begin
      T.Common.Current_Priority := Prio;
      Param.sched_priority := To_Target_Priority (Prio);
      Result :=
        pthread_setschedparam (T.Common.LL.Thread, SCHED_FIFO, Param'Access);
      pragma Assert (Result = 0);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
   begin
      Self_ID.Common.LL.Thread := pthread_self;
      Self_ID.Common.LL.LWP := lwp_self;

      Specific.Set (Self_ID);

      System.Float_Control.Reset;

      System.Init.Install_Handler;

      --  Register the task to System.Tasking.Debug

      System.Tasking.Debug.Add_Task_Id (Self_ID);

      --  If stack checking is enabled and limit checking is used, set the
      --  stack limit for this task. The environment task has this initialized
      --  by the binder-generated main when System.Stack_Check_Limits = True.

      if Self_ID /= Operations.Environment_Task
        and then Set_Stack_Limit_Hook /= null
      then
         Set_Stack_Limit_Hook.all;
      end if;
   end Enter_Task;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
      Mutex_Attr : aliased pthread_mutexattr_t;
      Result     : Interfaces.C.int;
      Result0    : Interfaces.C.int;
      Cond_Attr  : aliased pthread_condattr_t;

   begin
      Result := pthread_mutexattr_init (Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Result :=
           pthread_mutex_init
             (Self_ID.Common.LL.L'Access,
              Mutex_Attr'Access);
         pragma Assert (Result = 0 or else Result = ENOMEM);

         Result0 := pthread_mutexattr_destroy (Mutex_Attr'Access);
         pragma Assert (Result0 = 0);
      end if;

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then

         --  ??? Since we always use CLOCK_REALTIME, should be useless
         --  Result := GNAT_pthread_condattr_setup (Cond_Attr'Access);
         --  pragma Assert (Result = 0);

         Result :=
           pthread_cond_init (Self_ID.Common.LL.CV'Access, Cond_Attr'Access);
         pragma Assert (Result = 0 or else Result = ENOMEM);
      end if;

      if Result = 0 then
         Succeeded := True;
      else
         Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
         pragma Assert (Result = 0);
         Succeeded := False;
      end if;

      Result := pthread_condattr_destroy (Cond_Attr'Access);
      pragma Assert (Result = 0);
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Base_CPU   : System.Multiprocessors.CPU_Range;
      Succeeded  : out Boolean)
   is
      pragma Unreferenced (Base_CPU);
      Attributes          : aliased pthread_attr_t;
      Adjusted_Stack_Size : Interfaces.C.size_t;
      Page_Size           : constant Interfaces.C.size_t :=
                              Interfaces.C.size_t (Get_Page_Size);
      Result              : Interfaces.C.int;

      function Thread_Body_Access is new
        Ada.Unchecked_Conversion (System.Address, Thread_Body);

      use type Interfaces.C.size_t;
      use System.Task_Info;

   begin
      --  Add ~1/4 to requested stack size for secondary stack

      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Default_Stack_Size * 5) / 4);
      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Minimum_Stack_Size * 5) / 4);
      else
         Adjusted_Stack_Size :=
           System.OS_Interface.size_t ((Stack_Size * 5) / 4);
      end if;

      if Stack_Base_Available then

         --  If Stack Checking is supported then allocate 2 additional pages:

         --  In the worst case, stack is allocated at something like
         --  N * Get_Page_Size - epsilon, we need to add the size for 2 pages
         --  to be sure the effective stack size is greater than what
         --  has been asked.

         Adjusted_Stack_Size := Adjusted_Stack_Size + 2 * Page_Size;
      end if;

      --  Round stack size as this is required by some OSes (Darwin)

      Adjusted_Stack_Size := Adjusted_Stack_Size + Page_Size - 1;
      Adjusted_Stack_Size :=
        Adjusted_Stack_Size - Adjusted_Stack_Size mod Page_Size;

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result :=
        pthread_attr_setdetachstate
          (Attributes'Access, PTHREAD_CREATE_DETACHED);
      pragma Assert (Result = 0);

      Result :=
        pthread_attr_setstacksize (Attributes'Access, Adjusted_Stack_Size);
      pragma Assert (Result = 0);

      if T.Common.Task_Info /= Default_Scope then
         case T.Common.Task_Info is
            when System.Task_Info.Process_Scope =>
               Result :=
                 pthread_attr_setscope
                   (Attributes'Access, PTHREAD_SCOPE_PROCESS);

            when System.Task_Info.System_Scope =>
               Result :=
                 pthread_attr_setscope
                   (Attributes'Access, PTHREAD_SCOPE_SYSTEM);

            when System.Task_Info.Default_Scope =>
               Result := 0;
         end case;

         pragma Assert (Result = 0);
      end if;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we do
      --  not need to manipulate caller's signal mask at this point. All tasks
      --  in RTS will have All_Tasks_Mask initially.

      --  Note: the use of Unrestricted_Access in the following call is needed
      --  because otherwise we have an error of getting a access-to-volatile
      --  value which points to a non-volatile object. But in this case it is
      --  safe to do this, since we know we have no problems with aliasing and
      --  Unrestricted_Access bypasses this check.

      Result :=
        pthread_create
          (T.Common.LL.Thread'Unrestricted_Access,
           Attributes'Access,
           Thread_Body_Access (Wrapper),
           To_Address (T));
      pragma Assert (Result = 0 or else Result = EAGAIN);

      Succeeded := Result = 0;

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result = 0);

      if Succeeded then
         Set_Priority (T, Priority);
      end if;
   end Create_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : System.Tasking.Task_Id) is
   begin
      Specific.Initialize;

      --  Store the identifier for the environment task

      Operations.Environment_Task := Environment_Task;

      Enter_Task (Environment_Task);
   end Initialize;

   ---------------------
   -- Is_Task_Context --
   ---------------------

   function Is_Task_Context return Boolean is
   begin
      --  ??? TBI

      return True;
   end Is_Task_Context;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames Specific.Self;

end System.Task_Primitives.Operations;
