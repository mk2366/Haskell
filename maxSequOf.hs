import Data.List

maxLengthSequOf :: (Eq a) => a -> [a] -> Int
maxLengthSequOf a xs = maximum (0 : (map length . filter ((a ==) . head) . groupBy (==)) xs)
