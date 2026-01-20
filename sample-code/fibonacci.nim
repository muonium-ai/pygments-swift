# Nim Fibonacci
proc fib(n: int): int =
  if n < 2:
    return n
  fib(n-1) + fib(n-2)

let result = fib(10)
echo result
