#!/usr/bin/env bash
# Fibonacci sequence generator (iterative)
set -euo pipefail

n=12

a=0
b=1

out=()
for ((i=0; i<n; i++)); do
  out+=("$a")
  next=$((a + b))
  a=$b
  b=$next
done

(IFS=", "; echo "${out[*]}")
