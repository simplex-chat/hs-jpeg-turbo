#include <stdio.h>

int jsimd_can_rgb_ycc(void);

int main(void)
{
  puts(jsimd_can_rgb_ycc() ? "Neon" : "C");
  return 0;
}
