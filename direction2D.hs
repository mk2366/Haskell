data Direction = STRAIGHT |
                 RETURN   |
                 LEFT     |
                 RIGHT
                 deriving (Show)

type Point = (Float, Float)

angle :: Point -> Float
angle p@(x,y) | x < 0 = pi - angle ((-1)*x,y)
              | x == 0 = pi/2
              | otherwise = atan (y/x)

angle_between :: Point -> Point -> Float
angle_between p1@(x1,y1) p2@(x2,y2) = normalize (angle p2 - angle p1)
                                            where normalize a | a > pi = a - 2 * pi
                                                              | a <= (-1) * pi = a + 2 * pi
                                                              | otherwise = a
direction :: Point -> Point -> Point -> Direction
direction p1@(x1,y1) p2@(x2,y2) p3@(x3,y3)  | ang == 0 = STRAIGHT
                                            | ang == pi = RETURN
                                            | ang > 0 = LEFT
                                            | ang < 0 = RIGHT
                                               where ang = angle_between
                                                             (x2 - x1, y2 - y1)
                                                             (x3 - x2, y3 - y2)

directions :: [Point] -> [Direction]
directions (p1:p2:p3:xs) = (direction p1 p2 p3) : directions (p2:p3:xs)
directions   _           = []

--comparepoints

smallerelem (x1,y1) (x2,y2) | x1 < x2 =  True
                            | x1 == x2 = if (y1 < y2)
                                         then True
                                         else False
                            | otherwise = False

smallerangle (x1,y1) (x2,y2) | angle (x1,y1) < angle (x2,y2) =  True
                            | otherwise = False

sortelemTupleList :: (a -> a -> Bool) -> [a] -> [a]
sortelemTupleList f l@(x:xs) = sortelemTupleList f (filter (not . f x) xs) ++
                               x:sortelemTupleList f (filter (f x) xs)
sortelemTupleList _ [] = []

--convexhullsort (x:xs)
