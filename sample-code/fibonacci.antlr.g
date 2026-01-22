// Fibonacci grammar snippet (ANTLR)
grammar Fibonacci;

file: (stmt NEWLINE)* EOF;
stmt: 'fib' INT;

INT: [0-9]+;
NEWLINE: '\r'? '\n';
WS: [ \t]+ -> skip;

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
