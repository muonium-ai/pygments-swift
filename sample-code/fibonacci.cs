using System;
using System.Collections.Generic;

class Fibonacci {
    static List<int> Fib(int n) {
        var outList = new List<int>();
        int a = 0, b = 1;
        for (int i = 0; i < Math.Max(0, n); i++) {
            outList.Add(a);
            int next = a + b;
            a = b;
            b = next;
        }
        return outList;
    }

    static void Main() {
        int n = 12;
        Console.WriteLine(string.Join(", ", Fib(n)));
    }
}
