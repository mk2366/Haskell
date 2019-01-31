module Lib
    ( someFunc
    , maxLengthSequOf
    ) where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxLengthSequOf :: (Eq a) => a -> [a] -> Int
maxLengthSequOf a xs = maximum (0 : (map length . filter ((a ==) . head) . groupBy (==)) xs)
