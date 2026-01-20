# Fibonacci (PowerShell)

function Get-Fib([int]$n) {
  if ($n -lt 2) { return $n }
  return (Get-Fib ($n - 1)) + (Get-Fib ($n - 2))
}

0..11 | ForEach-Object {
  Write-Output "$_ $(Get-Fib $_)"
}
