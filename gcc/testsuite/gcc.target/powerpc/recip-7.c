/* { dg-do run { target { powerpc*-*-linux* powerpc*-ibm-aix* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target ppc_recip_hw } */
/* { dg-require-effective-target sqrt_insn } */
/* { dg-options "-O3 -ftree-vectorize -ffast-math -mrecip -mpowerpc-gfxopt -mpopcntb" } */

/* Check reciprocal estimate functions for accuracy.  */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <float.h>
#include <string.h>

#include "recip-test.h"
