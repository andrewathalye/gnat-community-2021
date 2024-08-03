/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                 C L E A R _ E X C E P T I O N _ C O U N T                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2004-2021, AdaCore                     *
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
 * It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). *
 *                                                                          *
 ****************************************************************************/

/* Clear vThreads exception count field for current task
   and reset the MSR on Vxworks 6 Cert for PPC.  */

/* This is a target-specific file for run-times that use the cert version of
   Ada.Exceptions.  */

/* This operation is implicit in the vThreads version of longjmp, but not in
   the gcc builtin_longjmp. This operation is performed in init.c for regular
   VxWorks vThreads run-time libraries.  */

/* For VxWorks Cert 6 on PPC this routine resets the FP bit in the MSR,
   which is necessary for stack limit checking to work using GCC FE
   exceptions.  */

/* The routine is also called in ravenscar-cert=* on LynxOS-178, but does
   nothing.  */

void
__gnat_clear_exception_count (void)
{
#if defined (__vxworks) && defined (CERT) && defined (__PPC__) && !defined (__SPE__) && !defined (__RTP__)
  register unsigned msr;
  /* Read the MSR value */
  asm volatile ("mfmsr %0" : "=r" (msr));
  /* Force the FP bit */
  msr |= 0x2000;
  /* Store to MSR */
  asm volatile ("mtmsr %0" : : "r" (msr));
#endif
}
