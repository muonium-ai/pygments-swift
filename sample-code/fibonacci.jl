# Fibonacci (Julia)

function fib(n)
    if n < 2
        return n
    end
    return fib(n - 1) + fib(n - 2)
end

for i in 0:11
    println(i, " ", fib(i))
end
