"Smalltalk sample"
Object subclass: #Fibonacci
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Demo'.

Fibonacci class >> fib: n
    n < 2 ifTrue: [ ^ n ].
    ^ (self fib: n - 1) + (self fib: n - 2).

Transcript show: (Fibonacci fib: 10) printString; cr.
