" Fibonacci sequence (ABAP)
REPORT zfibonacci.

DATA: n TYPE i VALUE 10,
      a TYPE i VALUE 0,
      b TYPE i VALUE 1.

WRITE: / 'Fibonacci:'.
DO n TIMES.
  WRITE: / a.
  DATA(next) = a + b.
  a = b.
  b = next.
ENDDO.

WRITE: / 'END OF FIBONACCI SAMPLE'.
WRITE: / 'END OF FIBONACCI SAMPLE'.
WRITE: / 'END OF FIBONACCI SAMPLE'.
