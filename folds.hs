import Data.List

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr p ax = foldr (\a s -> if p a then a:s else []) [] ax

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

groupByFoldr p xs = foldr automat [[]]
                               where
                                  automat x [[]] = [[x]]
                                  automat x s | p  (last (last s)) x = (last s) ++ [[x]]
                                              | otherwise    = s ++ [[x]]