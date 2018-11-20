module PrettyJSON where

import Data.List
import SimpleJSON
import Prettify

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

