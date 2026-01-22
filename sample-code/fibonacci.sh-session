$ echo "Fibonacci sequence"
Fibonacci sequence
$ python3 - <<'PY'
def fib(n):
    a, b = 0, 1
    for _ in range(n):
        print(a)
        a, b = b, a + b
fib(10)
PY
0
1
1
2
3
5
8
13
21
34

END OF FIBONACCI SAMPLE
END OF FIBONACCI SAMPLE
END OF FIBONACCI SAMPLE
