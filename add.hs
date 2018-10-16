fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

add :: Int -> Int
add x = x + 3


memoize :: (Num a, Num b) => (a -> b) -> (a -> b)
memoize (Int -> Int) = add
