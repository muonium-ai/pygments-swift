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
