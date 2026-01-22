module Fibonacci where

open import Agda.Builtin.Nat

fib : Nat -> Nat
fib zero = zero
fib (suc zero) = suc zero
fib (suc (suc n)) = fib (suc n) + fib n

-- END OF FIBONACCI SAMPLE
-- END OF FIBONACCI SAMPLE
-- END OF FIBONACCI SAMPLE
