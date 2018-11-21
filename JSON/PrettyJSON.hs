module PrettyJSON (renderJValue) where

import Data.List
import SimpleJSON (JValue (..))
import Prettify (Doc, text, double, string, series, (<+>))

renderJValue :: JValue     -> Doc
renderJValue JNull         = text "null"
renderJValue (JNumber n)   = double n
renderJValue (JString s)   = string s
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JObject o)   = series '{' '}' field o
        where field (s, jo) = text s <+> text ": " <+> renderJValue jo 
renderJValue (JArray a) = series '[' ']' renderJValue a

