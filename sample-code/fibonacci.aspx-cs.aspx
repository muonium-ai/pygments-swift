<%@ Page Language="C#" %>
<script runat="server">
  int Fib(int n) { return n <= 1 ? n : Fib(n-1) + Fib(n-2); }
  protected void Page_Load(object sender, EventArgs e) {
    for (int i = 0; i <= 10; i++) {
      Response.Write(Fib(i) + "<br/>");
    }
  }
</script>

<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
