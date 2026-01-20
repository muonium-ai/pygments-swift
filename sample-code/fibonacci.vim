" Fibonacci in Vimscript
function! Fib(n)
  if a:n < 2
    return a:n
  endif
  return Fib(a:n-1) + Fib(a:n-2)
endfunction

let g:result = Fib(10)
echo g:result
