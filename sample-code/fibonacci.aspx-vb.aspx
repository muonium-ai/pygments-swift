<%@ Page Language="VB" %>
<script runat="server">
  Function Fib(n As Integer) As Integer
    If n <= 1 Then Return n
    Return Fib(n-1) + Fib(n-2)
  End Function

  Sub Page_Load(sender As Object, e As EventArgs)
    For i As Integer = 0 To 10
      Response.Write(Fib(i).ToString() & "<br/>")
    Next
  End Sub
</script>

<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
