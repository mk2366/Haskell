{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Supply
    (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving (Monad,Applicative,Functor)

runSupply :: Supply s a -> [s] -> (a, [s])

next :: Supply s (Maybe s)

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs

doubleNext = liftM2 (,) next next