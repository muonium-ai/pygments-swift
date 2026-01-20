# AWK sample
function fib(n) {
  if (n < 2) return n
  return fib(n-1) + fib(n-2)
}
BEGIN {
  print "fib(10)=", fib(10)
}
