<?php
// Fibonacci sequence generator (iterative)

function fib(int $n): array {
    $out = [];
    $a = 0;
    $b = 1;
    for ($i = 0; $i < max(0, $n); $i++) {
        $out[] = $a;
        $next = $a + $b;
        $a = $b;
        $b = $next;
    }
    return $out;
}

$n = 12;
echo implode(", ", fib($n)) . "\n";
