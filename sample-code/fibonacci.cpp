// Fibonacci sequence generator (iterative)
#include <iostream>
#include <vector>

std::vector<long long> fib(int n) {
    std::vector<long long> out;
    out.reserve(n);
    long long a = 0;
    long long b = 1;
    for (int i = 0; i < n; i++) {
        out.push_back(a);
        long long next = a + b;
        a = b;
        b = next;
    }
    return out;
}

int main() {
    int n = 12;
    auto out = fib(n);
    for (size_t i = 0; i < out.size(); i++) {
        std::cout << out[i] << (i + 1 == out.size() ? "\n" : ", ");
    }
    return 0;
}
