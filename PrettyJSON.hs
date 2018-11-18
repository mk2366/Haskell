module PrettyJSON where

import Data.List
import SimpleJSON
import PrettifyStub

renderJValue :: JValue     -> Doc
renderJValue JNull         = text "null"
renderJValue (JNumber n)   = double n
renderJValue (JString s)   = string s
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
-- renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
--         where pairs [] = ""
--               pairs ps = intercalate ", " (map renderPair ps)
--               renderPair (k, v) = show k ++ ": " ++ renderJValue v
-- renderJValue (JArray a) = "[" ++ values a ++ "]"
--         where values [] = ""
--               values vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

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
        
              