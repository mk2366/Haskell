import Data.Char(toUpper)

main = do
    inputstr <- readFile "big.txt"
    writeFile "out" (map toUpper inputstr)