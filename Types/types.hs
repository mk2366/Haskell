{-# LANGUAGE NoImplicitPrelude #-}


module Types where
import Prelude(String, Double, Bool(..), Either(..), Eq, Show, Ord, id)

type JSONError = String

class JSON a where
    toJValue   :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue   = id
    fromJValue = Right

instance JSON Bool where
    toJValue             = JBool
    fromJValue (JBool b) = Right b
    fromJValue _         = Left "Not a JSON Bool"

newtype JAry a = JAry {
fromJAry :: [a]
} deriving (Eq, Show, Ord)

newtype JObj a = JObj {
fromJObject :: [(String, a)]
} deriving (Eq, Show, Ord)

data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject (JObj JValue)
            | JArray  (JAry JValue)
            deriving (Eq, Show, Ord) 