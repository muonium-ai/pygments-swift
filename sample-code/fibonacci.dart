// Fibonacci (Dart)

int fib(int n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

void main() {
  for (var i = 0; i < 12; i++) {
    print('$i ${fib(i)}');
  }
}
