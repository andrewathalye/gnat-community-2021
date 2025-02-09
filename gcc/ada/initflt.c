/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                              I N I T F L T                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2003-2021, Free Software Foundation, Inc.       *
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

/* Initialize floating point.  For the restricted / cert runtime */

void
__gnat_init_float ()
{
  /* Disable overflow/underflow exceptions on the PPC processor, this is needed
     to get correct Ada semantic.  */
#if defined (_ARCH_PPC) && !defined (_SOFT_FLOAT) && (!defined (VTHREADS) || defined (__VXWORKSMILS__))
#if defined (__SPE__)
  {
    /* For e500v2, do nothing and leave the responsibility to install the
       handler and enable the exceptions to the BSP.  */
  }
#else
  __asm__ ("mtfsb0 25");
  __asm__ ("mtfsb0 26");
#endif

#else
#if (defined (__i386__) || defined (__x86_64__)) && !defined (VTHREADS)
  /* This is used to properly initialize the FPU to 64-bit precision on an x86
     for each process thread and also for floating-point I/O.  */
  asm ("finit");
#endif
#endif
}
