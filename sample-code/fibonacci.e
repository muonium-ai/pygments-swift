-- Eiffel sample
class
    FIBONACCI

create
    make

feature {NONE}
    make
        local
            n: INTEGER
        do
            n := 10
            print ("fib=" + fib (n).out + "%N")
        end

    fib (n: INTEGER): INTEGER
        do
            if n < 2 then
                Result := n
            else
                Result := fib (n - 1) + fib (n - 2)
            end
        end
end
