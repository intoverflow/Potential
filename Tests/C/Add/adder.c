/* gcc adder.c add2.S */

#import <stdio.h>

long add2(long a, long b);

long add1(long a, long b) {
  return a + b;
}

int main() {
  long a = 3;
  long b = 4;
  long c1 = 0;
  long c2 = 0;

  c1 = add1(a, b);
  c2 = add2(a, b);

  printf("add1: %ld\n", c1);
  printf("add2: %ld\n", c2);

  return 0;
}

