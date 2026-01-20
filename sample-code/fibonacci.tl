-- fibonacci in Teal
local function fib(n: integer): integer
   if n < 2 then
      return n
   end
   return fib(n - 1) + fib(n - 2)
end
