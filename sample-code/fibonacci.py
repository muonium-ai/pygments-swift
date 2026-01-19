# Fibonacci sequence generator (iterative)

def fib(n: int) -> list[int]:
    out: list[int] = []
    a, b = 0, 1
    for _ in range(max(0, n)):
        out.append(a)
        a, b = b, a + b
    return out


if __name__ == "__main__":
    n = 12
    print(", ".join(map(str, fib(n))))
