// fibonacci in YARA
rule Fibonacci {
  meta:
    author = "pygments-swift"
  strings:
    $a = "fib" nocase
  condition:
    $a
}
