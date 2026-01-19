# Fibonacci sequence generator (iterative)

def fib(n)
  out = []
  a = 0
  b = 1
  [n, 0].max.times do
    out << a
    a, b = b, a + b
  end
  out
end

n = 12
puts fib(n).join(", ")
