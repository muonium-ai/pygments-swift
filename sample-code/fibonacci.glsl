// GLSL sample
#version 330 core

int fib(int n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

void main() {
    int x = fib(10);
}
