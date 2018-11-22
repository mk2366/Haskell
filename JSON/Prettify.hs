{-# LANGUAGE NoImplicitPrelude #-}

module Prettify (Doc,
                 empty,
                 char,
                 text,
                 double,
                 line,
                 compact,
                 series,
                 (<>),
                 string
                 )
      where

import Prelude (Show, String, Char, Int, Maybe (..), Double, map, (.), otherwise, (-), (+),
                (<), (>), (==), (||), length, replicate, zipWith, undefined, lookup, show,
                foldr, (++))      

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

data Doc = Empty          |
           Char Char      |
           Text String    |
           Line           |
           Concat Doc Doc |
           Union  Doc Doc
     deriving (Show)

empty :: Doc
empty = Empty

text   :: String -> Doc
text "" = Empty
text x  = Text x

double :: Double -> Doc
double num = Text (show num)

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

-- This is the coolest function I ever saw
compact :: Doc -> String
compact x = transform [x]
              where transform []      = ""
                    transform (d:ds)  = 
                       case d of 
                             Empty        -> transform ds
                             Char c       -> c:transform ds
                             Text s       -> s ++ transform ds
                             Line         -> '\n' : transform ds
                             a `Concat` b -> transform (a:b:ds)
                             _ `Union`  b -> transform (b:ds)


hcat :: [Doc] -> Doc
hcat = foldr (<>) Empty

(<>) :: Doc -> Doc -> Doc
Empty <> d     = d
d     <> Empty = d
d1    <> d2    = d1 `Concat` d2  

fsep :: [Doc] -> Doc
fsep = foldr (</>) Empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group Line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (Concat x y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
                  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
                  
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
            where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0')
                        <> text h
                  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
                  where a = (n `shiftR` 10) .&. 0x3ff
                        b = n .&. 0x3ff
 
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000) 
               where d = ord c                       

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',')
                               . map item

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate a [x] = [x]
punctuate a (d:ds) = (d <> a) : punctuate a ds 