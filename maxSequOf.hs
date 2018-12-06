import Data.List

maxLengthSequOf :: (Eq a) => a -> [a] -> Int
maxLengthSequOf a xs = maximum (0 : (map length . filter ((a ==) . head) . groupBy (==)) xs)

--P5M :: Int -> Int
-- addDiceThrow combinations = foldr combinator [] combinations
--                             where combinator wurf z = 

newThrowForExistingCombination :: (Num a, Enum a) => [a] -> [[a]]
newThrowForExistingCombination [] = map (\x -> [x]) [1..6]
newThrowForExistingCombination c = map (\(ls,l) -> l:ls) (zip (repeat c) [1..6])

allThrowCombinationsSumGreaterM n m = foldr (combinator m n) [[]] [1..n]

combinator :: Int -> Int -> Int -> [[Int]] -> [[Int]] 
combinator m n j cs = generateNew cs
            where generateNew cs =  filter (\xs -> sum xs >= (m - 6*(n-j+1) ))
                                           (foldr (++) [] (map newThrowForExistingCombination cs))

-- 