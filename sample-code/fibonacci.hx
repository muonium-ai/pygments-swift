package;

class Main {
  static function fib(n:Int):Int {
    if (n < 2) return n;
    return fib(n-1) + fib(n-2);
  }

  static function main() {
    trace(fib(10));
  }
}

// End of fibonacci sample
