module SimpleJSON 
    (
        JValue (..),
        getString,
        getInt,
        getDouble,
        getBool,
        getObject,
        getArray,
        isNull
    ) where

data JValue = 
    JString  String  |
    JNumber Double  |
    JBool   Bool    |
    JNull           |
    JObject [(String, JValue)] |
    JArray [JValue]
    deriving (Eq, Ord, Show)

getString :: JValue   -> Maybe String
getString (JString s) =  Just s
getString _           =  Nothing

getInt    :: JValue   -> Maybe Int
getInt    (JNumber n) = Just (truncate n)
getInt    _           = Nothing

getDouble :: JValue   -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _           = Nothing

getBool   (JBool b)   = Just b
getBool   _           = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray js)  = Just js
getArray _            = Nothing

isNull   :: JValue    -> Bool 
isNull   v            = v == JNull