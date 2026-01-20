' VBScript sample
Dim n
n = 10

Function fib(k)
  If k < 2 Then
    fib = k
  Else
    fib = fib(k-1) + fib(k-2)
  End If
End Function

WScript.Echo "fib(10)=" & fib(n)
