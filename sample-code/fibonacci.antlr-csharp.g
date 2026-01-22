// Fibonacci grammar snippet (ANTLR C# target)
grammar FibonacciCSharp;

@header { namespace Fib; }

start: 'fib' INT EOF;
INT: [0-9]+;
WS: [ \t\r\n]+ -> skip;

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
