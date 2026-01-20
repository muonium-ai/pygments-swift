-- Ada sample
with Ada.Text_IO;

procedure Fibonacci is
   function Fib(N : Integer) return Integer is
   begin
      if N < 2 then
         return N;
      else
         return Fib(N - 1) + Fib(N - 2);
      end if;
   end Fib;

begin
   Ada.Text_IO.Put_Line("fib(10)=" & Integer'Image(Fib(10)));
end Fibonacci;
