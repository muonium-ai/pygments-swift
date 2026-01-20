' Visual Basic sample
Imports System

Module Program
    Function Fib(n As Integer) As Integer
        If n < 2 Then
            Return n
        End If
        Return Fib(n - 1) + Fib(n - 2)
    End Function

    Sub Main()
        Console.WriteLine("fib(10)=" & Fib(10))
    End Sub
End Module
