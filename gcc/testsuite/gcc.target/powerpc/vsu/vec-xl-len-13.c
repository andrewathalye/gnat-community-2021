/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -Werror=implicit-function-declaration" } */

#include <stddef.h>
#include <altivec.h>

/* This test only runs on 32-bit configurations, where a compiler
   error should be issued because this built-in function is not
   available on 32-bit configurations. */

void
fetch_data (float *address, size_t length)
{
  __builtin_vec_lxvl (address, length);	/* { dg-error "'__builtin_vec_lxvl' is not supported in this compiler configuration|implicit declaration of function '__builtin_vec_lxvl'; did you mean .*" } */
}
