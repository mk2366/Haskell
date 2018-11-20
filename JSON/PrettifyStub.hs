module PrettifyStub where

data Doc = ToBeDefined
     deriving (Show)

text   :: String -> Doc
text   str = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

hexEscape :: Char -> Doc
hexEscape c = undefined

(<>) :: Doc -> Doc -> Doc
d1 <> d2 = undefined