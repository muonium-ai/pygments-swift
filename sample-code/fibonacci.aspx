<%@ Page Language="C#" %>
<%@ Import Namespace="System" %>

<!DOCTYPE html>
<html>
<head>
  <title>Fibonacci</title>
</head>
<body>
<%
  int n = 10;
  int a = 0;
  int b = 1;
%>

<h1>Fibonacci</h1>
<ul>
<%
  for (int i = 0; i < n; i++) {
%>
  <li><%= a %></li>
<%
    int t = a + b;
    a = b;
    b = t;
  }
%>
</ul>

<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
<!-- END OF FIBONACCI SAMPLE -->
</body>
</html>
