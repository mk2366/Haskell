module Main (main) where

import SimpleJSON
import PutJSON

main = putJValue (JObject [("foo", JNumber 1), ("Bar", JBool False)])