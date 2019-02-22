{-# LANGUAGE ScopedTypeVariables #-}


import Data.Ratio
import Data.List
import Control.Monad

oneRoll :: [(Integer, Ratio Integer)]
oneRoll = zip [1..6] $ repeat $ 1%6

-- if P(n|m) denotes the probability to throw exactly value n
-- with m dices then
-- [P(n|m)] = ps!!m gives a list for all possible outcomes
ps :: [[(Integer, Ratio Integer)]]
ps = oneRoll : (zipWith fancy ps $ repeat oneRoll)

fancy :: [(Integer, Ratio Integer)] -> [(Integer,Ratio Integer)] -> [(Integer ,Ratio Integer)]
fancy lastP next = 
    fmap (foldr (\(n,p) (_, ps) -> (n, p + ps)) (0,0%1)) 
    . groupBy (\(a,b) (c,d) -> a==c)
    . sort $ 
    do 
      (l1, l2) <- lastP
      (n1, n2) <- next
      return $ (l1 + n1, l2 * n2)

-- psGT n m will give you the probability to have more or equal than m with n dices
psGt :: Integer -> Integer -> Ratio Integer
psGt n m = sum . fmap snd $ filter ((>= m) . fst) $ ps !! (fromIntegral n - 1)
