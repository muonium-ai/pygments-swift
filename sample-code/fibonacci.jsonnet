// fibonacci in Jsonnet
local fib(n) = if n < 2 then n else fib(n - 1) + fib(n - 2);
{
  fib10: fib(10),
  msg: "ok",
}
