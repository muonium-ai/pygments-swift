// Fibonacci sequence (AspectJ)
public aspect FibonacciAspect {
  public static int fib(int n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
  }

  public static void main(String[] args) {
    for (int i = 0; i <= 10; i++) {
      System.out.println(fib(i));
    }
  }
}

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
