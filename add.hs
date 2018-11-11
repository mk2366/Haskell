import Data.Typeable

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

add :: Int -> Int
add x = x + 3

sub :: Integer -> Integer
sub x = x - 3


memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

ls1 :: Eq a => a -> [a] -> Int
ls1 _ [] = 0
ls1 a s@(x:xs) | x /= a = ls1 a xs

type CustomerId = Int
type Review = String
data DReview = DReview CustomerId Review deriving (Show)

x = DReview 4 "Me"

type Point = (Float, Float)

direction :: Point -> Point -> Point -> Float
direction a@(ax,ay) b@(bx,by) c@(cx,cy)   | a==b || b==c = error("Punkte identisch")
                                          | signum (bx - ax) > 0 && 
                                          -- | otherwise = acos (((bx - ax) * (cx - bx) + (by - ay) * (cy - by)) / ((l s1) * (l s2)))
                                          --                   where s1 = ((bx - ax), (by - ay))
                                          --                         s2 = ((cx - bx), (cy - by))
                                          --                         l (a,b) = sqrt (a*a + b*b)

data Direction =   LEFT
                 | RIGHT
                 | STRAIGHT
                 | TURN
                 deriving (Show)
