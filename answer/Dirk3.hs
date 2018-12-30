import Data.List (delete, minimumBy)
import Data.List.Index (indexed)

compare' :: Ord a => (Int, a) -> (Int, a) -> Ordering
compare' (a, b) (c,d) = compare b d 

pullNearestPoint :: (Ord a, Fractional a) => a -> [a] -> Maybe (a,[a])
pullNearestPoint _ [] = Nothing
pullNearestPoint p xs = let
                           xs'     = map (abs . (-)p) xs
                           ixs     = indexed xs'
                           (i, _)  = minimumBy compare' ixs
                           a       = xs !! i
                           xs''    = delete (xs !! i) xs
                           in Just (a, xs'')
