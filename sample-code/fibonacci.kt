// Fibonacci sequence generator (iterative)

fun fib(n: Int): List<Int> {
    val out = mutableListOf<Int>()
    var a = 0
    var b = 1
    repeat(kotlin.math.max(0, n)) {
        out.add(a)
        val next = a + b
        a = b
        b = next
    }
    return out
}

fun main() {
    val n = 12
    println(fib(n).joinToString(", "))
}
