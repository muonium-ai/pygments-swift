# Fibonacci (Elixir)

defmodule Fib do
  def fib(n) when n < 2, do: n
  def fib(n), do: fib(n - 1) + fib(n - 2)
end

Enum.each(0..11, fn i ->
  IO.puts("#{i} #{Fib.fib(i)}")
end)
