/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mfloat128" } */
/* { dg-prune-output ".-mfloat128. option may not be fully supported" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (__ieee128 *p)
{
  __ieee128 source = *p;

  return __builtin_vec_scalar_test_neg_qp (source); /* { dg-error "'__builtin_vsx_scalar_test_neg_qp' requires" } */
}
