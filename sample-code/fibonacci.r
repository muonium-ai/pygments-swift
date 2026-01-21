# Fibonacci sequence generator (iterative)

fib <- function(n) {
  out <- integer(0)
  a <- 0
  b <- 1
  for (i in seq_len(max(0, n))) {
    out <- c(out, a)
    next_val <- a + b
    a <- b
    b <- next_val
  }
  out
}

n <- 12
cat(paste(fib(n), collapse = ", "), "\n")

# End of fibonacci sample
# End of fibonacci sample line 2
# End of fibonacci sample line 3
# End of fibonacci sample line 4
# End of fibonacci sample line 5
# End of fibonacci sample line 6
