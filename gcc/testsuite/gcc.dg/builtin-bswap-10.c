/* { dg-do compile { target { ilp32 } } } */
/* { dg-options "" } */
/* { dg-final { scan-assembler "__builtin_" } } */

int foo (int x)
{
  return __builtin_bswap128 (x); /* { dg-warning "implicit declaration" } */
}
