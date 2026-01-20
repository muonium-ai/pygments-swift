# Fibonacci (Markdown)

This file exercises a **minimal** Markdown lexer.

- Inline code: `fib(10)`
- Link: [Pygments](https://pygments.org)

```swift
func fib(_ n: Int) -> Int {
    if n < 2 { return n }
    return fib(n - 1) + fib(n - 2)
}

for i in 0..<12 {
    print(i, fib(i))
}
```

> Quote block to check `>` handling.
