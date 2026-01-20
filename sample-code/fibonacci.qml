// Fibonacci QML
import QtQuick 2.0

Item {
  property int n: 10
  function fib(x) { if (x < 2) return x; return fib(x-1) + fib(x-2); }
}
