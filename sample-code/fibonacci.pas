{ Pascal sample }
program Fibonacci;

function fib(n: Integer): Integer;
begin
  if n < 2 then
    fib := n
  else
    fib := fib(n-1) + fib(n-2);
end;

begin
  writeln('fib(10)=', fib(10));
end.
