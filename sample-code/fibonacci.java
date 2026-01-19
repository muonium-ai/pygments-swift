import java.util.ArrayList;
import java.util.List;

public class Fibonacci {
    public static List<Integer> fib(int n) {
        List<Integer> out = new ArrayList<>();
        int a = 0;
        int b = 1;
        for (int i = 0; i < Math.max(0, n); i++) {
            out.add(a);
            int next = a + b;
            a = b;
            b = next;
        }
        return out;
    }

    public static void main(String[] args) {
        int n = 12;
        System.out.println(fib(n));
    }
}
