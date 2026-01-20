/* comment */
module main;
import std.stdio;

int fib(int n) {
  if (n < 2) return n;
  return fib(n-1) + fib(n-2);
}

void main() {
  writeln(fib(10));
}
