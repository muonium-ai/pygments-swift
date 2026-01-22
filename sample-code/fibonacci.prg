* Fibonacci (FoxPro / xBase)
PROCEDURE fib
  PARAMETERS n
  LOCAL a, b, i, t
  a = 0
  b = 1
  FOR i = 1 TO n
    ? a
    t = a + b
    a = b
    b = t
  ENDFOR
  RETURN .t.
ENDPROC

&& END OF FIBONACCI SAMPLE
&& END OF FIBONACCI SAMPLE
&& END OF FIBONACCI SAMPLE
