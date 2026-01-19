package main

import (
	"fmt"
	"strings"
)

func fib(n int) []int {
	out := make([]int, 0, n)
	a, b := 0, 1
	for i := 0; i < n; i++ {
		out = append(out, a)
		a, b = b, a+b
	}
	return out
}

func main() {
	n := 12
	values := fib(n)
	parts := make([]string, 0, len(values))
	for _, v := range values {
		parts = append(parts, fmt.Sprintf("%d", v))
	}
	fmt.Println(strings.Join(parts, ", "))
}
