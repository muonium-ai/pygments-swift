# comment

def fib(n : Int32) : Int32
  if n < 2
    n
  else
    fib(n-1) + fib(n-2)
  end
end

puts fib(10)
