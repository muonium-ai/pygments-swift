// Fibonacci (OpenSCAD)
function fib(n) = n < 2 ? n : fib(n - 1) + fib(n - 2);

for (i = [0 : 10]) {
  echo(i, fib(i));
}

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
