// Fibonacci grammar snippet (ANTLR ActionScript target)
grammar FibonacciActionScript;

@header { package fib; }

start: 'fib' INT EOF;
INT: [0-9]+;
WS: [ \t\r\n]+ -> skip;

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
