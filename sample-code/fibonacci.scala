// Fibonacci sequence generator (iterative)

object Fibonacci {
  def fib(n: Int): List[Int] = {
    var a = 0
    var b = 1
    var out: List[Int] = Nil

    for (_ <- 0 until math.max(0, n)) {
      out = out :+ a
      val next = a + b
      a = b
      b = next
    }

    out
  }

  def main(args: Array[String]): Unit = {
    val n = 12
    println(fib(n).mkString(", "))
  }
}
