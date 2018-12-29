import Data.List
import Data.Tuple

vowels :: [Char]
vowels = "aeiou"    --List of vowel characters

getLetterCount :: Char -> String -> Int
getLetterCount c s = length (filter (\x -> x == c) s)

getLetterTuple :: String -> Char -> (Char, Int)
getLetterTuple s c = (c, (getLetterCount c s))

getVowelCount :: String -> [(Char, Int)]
getVowelCount s = map (getLetterTuple s) vowels

main = do
 putStrLn $ show $ getVowelCount "many vowels in this sentence"