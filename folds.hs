import Data.List

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr p ax = foldr (\a s -> if p a then a:s else []) [] ax

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

groupByFoldr p = foldr automat [[]]
                               where
                                  automat x [[]] = [[x]]
                                  automat x l@((s:ss):ls)
                                              | p x s = (x:s:ss):ls
                                              | otherwise = [x]:l

anyFoldr :: Foldable t => (a -> Bool) -> t a -> Bool
anyFoldr p = foldr (\a s -> if p a then True else False || s) False

cycleFoldr :: [a] -> [a]
cycleFoldr as = foldr automat as [1..]
                   where automat _ s = as ++ s
