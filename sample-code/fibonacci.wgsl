// fibonacci in WGSL
fn fib(n: i32) -> i32 {
  if (n < 2) { return n; }
  return fib(n - 1) + fib(n - 2);
}

@compute @workgroup_size(1)
fn main() {
  let x: i32 = fib(10);
}
