# comment
function fib
    set -l n $argv[1]
    if test $n -lt 2
        echo $n
    else
        math (fib (math $n - 1)) + (fib (math $n - 2))
    end
end

echo (fib 10)
