addieren :: Int -> Int -> Int
addieren a b=a+b
multiplizieren :: Int -> Int -> Int
multiplizieren x y=x*y
cool :: Int -> Int
cool = multiplizieren 11


mitternacht :: Floating b => b -> b -> b -> (b, b)
mitternacht a b c = ((-1 * b + d) / (2 * a), (-1 * b - d) / (2 * a))
                    where d = sqrt(b ^ 2 - 4 * a * c)

heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt (s * (s-a) * (s-b) * (s-c))
                    where s = (a + b + c) / 2

ack a 0 = a +1
ack 0 a = ack 1 (a - 1)
ack a b = ack (ack (a-1) b)  (b -1)

 
lol 1=1
lol n=n*lol (n-1)
