import Data.List

maxLengthSequOf :: (Eq a, Num a) => a -> [a] -> Int
maxLengthSequOf a = maximum' . map length . filter ((a ==) . head) . groupBy (==)
                         where maximum' [] = 0
                               maximum' xs = maximum xs
