/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details -fdump-tree-optimized" } */
/* { dg-add-options sqrt_insn } */
/* { dg-final { scan-tree-dump "cdce3.c:11: \[^\n\r]* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump "sqrtf \\(\[^\n\r]*\\); \\\[tail call\\\]" "optimized" } } */

float sqrtf (float);
float foo (float x)
{
  return sqrtf (x);
}

