/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mvzeroupper" } */

int*
find_ptr (int* mem, int sz, int val)
{
  for (int i = 0; i < sz; i++)
    if (mem[i] == val) 
      return &mem[i];
  return 0;
}

/* { dg-final { scan-assembler-times "xorl\[\\t \]*\\\%e\[ab\]x,\[\\t \]*%e\[ab\]x" 1 } } */
