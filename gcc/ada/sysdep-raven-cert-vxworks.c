/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                                S Y S D E P                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 2009-2021, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains system dependent symbols that are referenced in the
   GNAT Run Time Library - Version for Ravenscar Cert VxWorks */

#include "vxWorks.h"
#include <taskLib.h>
#include <signal.h>

#ifdef __RTP__
/* For VxWorks Cert RTPs we create tasks using taskOpen, since taskSpawn is not
   supported for RTPs in the cert kernel. For VxWorks Cert kernel tasks,
   VxWorks 653 and VxWorks MILS vThreads, we still use taskSpawn (taskOpen is
   not provided).
*/

#include <objLibCommon.h>

extern int __gnat_get_object_options (void);
/* __gnat_get_object_options is used by s-taprop.adb only for VxWorks Cert.
   This function returns the value of options OM_CREATE | OM_EXCL, causing a
   new task to be created unconditionally without searching for a task of the
   same name. We use the VxWorks headers as the values of the flags may change
   across VxWorks versions.
*/

int
__gnat_get_object_options (void)
{
  int options = OM_CREATE | OM_EXCL;
  return options;
}
#endif

extern int __gnat_get_task_options (void);
/* __gnat_get_task_options is used by s-taprop.adb only for VxWorks. This
   function returns the options to be set when creating a new task. It fetches
   the options assigned to the current task (parent), so offering some user
   level control over the options for a task hierarchy. It forces VX_FP_TASK
   because it is almost always required.

*/int
__gnat_get_task_options (void)
{
  int options;

#ifdef VTHREADS
  /* Get the options for the task creator */
  taskOptionsGet (taskIdSelf (), &options);

#elif (!defined (__RTP__))
  /* taskOptionsGet not available on VxWorks Cert 6 */
  options = VX_SUPERVISOR_MODE | VX_STDIO;

#else
  options = 0;
#endif

  /* Force VX_FP_TASK or VX_SPE_TASK as needed */
#if defined (__SPE__)
  options |= VX_SPE_TASK;
#else
  options |= VX_FP_TASK;
#endif

  /* Mask those bits that are not under user control */
#ifdef VX_USR_TASK_OPTIONS
  return options & VX_USR_TASK_OPTIONS;
#else
  return options;
#endif
}
