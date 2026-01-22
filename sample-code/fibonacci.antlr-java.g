// Fibonacci grammar snippet (ANTLR Java target)
grammar FibonacciJava;

@header { package fib; }

start: 'fib' INT EOF;
INT: [0-9]+;
WS: [ \t\r\n]+ -> skip;

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
