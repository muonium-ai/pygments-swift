# Fibonacci (Mojo)
fn fib(n: Int) -> Int:
    var a = 0
    var b = 1
    for i in range(n):
        print(a)
        let t = a + b
        a = b
        b = t
    return a

fn main():
    print(fib(10))

# END OF FIBONACCI SAMPLE
# END OF FIBONACCI SAMPLE
# END OF FIBONACCI SAMPLE
