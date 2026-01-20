// Fibonacci (Groovy)

def fib(int n) {
  if (n < 2) return n
  fib(n - 1) + fib(n - 2)
}

(0..11).each { i ->
  println("$i ${fib(i)}")
}
