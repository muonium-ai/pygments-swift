import Foundation

// Fibonacci sequence generator (iterative)
func fib(_ n: Int) -> [Int] {
    guard n > 0 else { return [] }
    var out: [Int] = []
    out.reserveCapacity(n)

    var a = 0
    var b = 1
    for _ in 0..<n {
        out.append(a)
        (a, b) = (b, a + b)
    }
    return out
}

let n = 12
print(fib(n).map(String.init).joined(separator: ", "))
