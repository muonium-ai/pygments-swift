-- Fibonacci (Haskell)

fib :: Int -> Int
fib n | n < 2 = n
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = mapM_ (\i -> putStrLn (show i ++ " " ++ show (fib i))) [0..11]
