// Fibonacci sequence generator (iterative)
#include <stdio.h>

int main(void) {
    int n = 12;
    int a = 0;
    int b = 1;

    for (int i = 0; i < n; i++) {
        printf("%d", a);
        if (i != n - 1) {
            printf(", ");
        }
        int next = a + b;
        a = b;
        b = next;
    }
    printf("\n");
    return 0;
}
