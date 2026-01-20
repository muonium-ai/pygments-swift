// fibonacci in WebIDL
[Exposed=Window]
interface Fibonacci {
  attribute long n;
  long fib(long n);
};
