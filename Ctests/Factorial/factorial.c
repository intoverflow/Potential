/* gcc factorial.c doFactorial.S */

#import <stdio.h>

long doFactorial(long n);

int main() {
  long a1 = 1;
  long b1 = 3;
  long c1 = 5;

  long a2;
  long b2;
  long c2;

  a2 = doFactorial(a1);
  b2 = doFactorial(b1);
  c2 = doFactorial(c1);

  printf("%ld! = %ld\n", a1, a2);
  printf("%ld! = %ld\n", b1, b2);
  printf("%ld! = %ld\n", c1, c2);

  return 0;
}

