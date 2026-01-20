// SystemVerilog sample
module fib;
  function automatic int f(input int n);
    if (n < 2) return n;
    return f(n-1) + f(n-2);
  endfunction

  initial begin
    $display("fib(10)=%0d", f(10));
  end
endmodule
