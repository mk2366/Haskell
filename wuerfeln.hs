import Data.List

newThrowForExistingCombination :: (Num a, Enum a) => [a] -> [[a]]
newThrowForExistingCombination [] = map (\x -> [x]) [1..6]
newThrowForExistingCombination c = map (\(ls,l) -> l:ls) (zip (repeat c) [1..6])

allThrowCombinationsSumGreaterM n m = foldr (combinator m n) [[]] (reverse [1..n])

combinator :: Int -> Int -> Int -> [[Int]] -> [[Int]] 
combinator m n j cs = generateNew cs
            where generateNew cs =  filter (\xs -> sum xs >= (m - 6*(n-j) ))
                                           (foldr (++) [] (map newThrowForExistingCombination cs))

main = writeFile "out" (show (allThrowCombinationsSumGreaterM 10 30))