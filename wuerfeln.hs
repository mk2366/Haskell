import Data.List
import Data.Ratio
import System.Environment

newThrowForExistingCombination :: (Num a, Enum a) => [a] -> [[a]]
newThrowForExistingCombination [] = map (\x -> [x]) [1..6]
newThrowForExistingCombination c = map (\(ls,l) -> l:ls) (zip (repeat c) [1..6])

allThrowCombinationsSumGreaterM n m = foldr (combinator m n) [[]] (reverse [1..n])

pNM :: Int -> Int -> Ratio Int
pNM n m | n > 22 || n < 1 = error "Maximal 22 Würfel und mindestens 1 :-)"
        | otherwise       = fromIntegral (((length.allThrowCombinationsSumGreaterM n ) m)) / (6 ^ n)

pNM' :: [String] -> Ratio Int
pNM' (ns:ms:whatever) = pNM (read ns) (read ms)

combinator :: Int -> Int -> Int -> [[Int]] -> [[Int]] 
combinator m n j cs = generateNew cs
            where generateNew cs =  filter (\xs -> sum xs >= (m - 6*(n-j) ))
                                           (foldr (++) [] (map newThrowForExistingCombination cs))

main = getArgs >>= print . pNM'

--
