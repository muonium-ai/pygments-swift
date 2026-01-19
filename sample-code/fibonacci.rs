// Fibonacci sequence generator (iterative)

fn fib(n: usize) -> Vec<u64> {
    let mut out: Vec<u64> = Vec::with_capacity(n);
    let mut a: u64 = 0;
    let mut b: u64 = 1;
    for _ in 0..n {
        out.push(a);
        let next = a + b;
        a = b;
        b = next;
    }
    out
}

fn main() {
    let n = 12;
    let out = fib(n);
    let s = out
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    println!("{}", s);
}
