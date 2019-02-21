{-# LANGUAGE ScopedTypeVariables #-}


import Data.Ratio

-- if P(n|m) denotes the probability to throw exactly value n
-- with m dices then
-- P(n|m) = ps!!m!!n
ps :: [[Ratio Int]]
ps = (take 6 $ repeat $ 1%6 :: [Ratio Int]) : (last ps)