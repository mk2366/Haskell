{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}


module Types where
import Prelude(String, Double, Bool(..), Either(..), Eq, Show, Ord, id, undefined, error, (.), map,
               (*), ($), Int, Integer, realToFrac, round)
import Control.Arrow (second)

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

instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "Not a JSON String"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
-- instance {-# OVERLAPPABLE #-} (JSON a) => JSON [a] where
--     toJValue               = undefined
--     fromJValue             = undefined

-- instance {-# OVERLAPPABLE #-} (JSON a) => JSON [(String, a)] where
--     toJValue               = error ("What?")
--     fromJValue              = undefined

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = jaryOfJValuesToJValue . JAry. listToJValues . fromJAry

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _                 = Left "Not a JSON Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (a:as) = case (mapEithers f as) of
                         (Left e) -> Left e
                         (Right rs) -> case f a of
                                 (Left e) -> Left e
                                 (Right r) -> Right (r:rs)
mapEithers _ []     = Right []

                         
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: (JAry JValue) -> JValue
jaryOfJValuesToJValue = JArray

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue


-- JObject
instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObject
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
                                       where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _                  = Left "Not a JSON object"

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