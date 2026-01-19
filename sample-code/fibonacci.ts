// Fibonacci sequence generator (iterative)

function fib(n: number): number[] {
  const out: number[] = [];
  let a = 0;
  let b = 1;
  for (let i = 0; i < Math.max(0, n); i++) {
    out.push(a);
    [a, b] = [b, a + b];
  }
  return out;
}

const n = 12;
console.log(fib(n).join(", "));
