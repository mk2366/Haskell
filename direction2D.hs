import Data.List
import Data.Char

data Direction = STRAIGHT |
                 RETURN   |
                 LEFT     |
                 RIGHT
                 deriving (Show, Eq)

type Point = (Double, Double)

angle :: Point -> Double
angle p@(x,y) | x < 0 = pi - angle ((-1)*x,y)
              | x == 0 = pi/2
              | otherwise = atan (y/x)

angle_between :: Point -> Point -> Double
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
directions (p1:p2:p3:xs) = direction p1 p2 p3 : directions ( p2:p3:xs )
directions   _           = []

--comparepoints

smallerelem (x1,y1) (x2,y2) | y1 < y2 =  True
                            | y1 == y2 = x1 < x2
                            | otherwise = False

smallerangle (px, py) (x1,y1) (x2,y2) | angle (x1 - px,y1 - py) < angle (x2 - px, y2 - py) = True
                                      | otherwise = False

sortelemTupleList :: (a -> a -> Bool) -> [a] -> [a]
sortelemTupleList f l@(x:xs) = sortelemTupleList f (filter (not . f x) xs) ++
                               x:sortelemTupleList f (filter (f x) xs)
sortelemTupleList _ [] = []

graham_scan_adjust_stack :: Point -> [Point] -> [Point]
graham_scan_adjust_stack p (p1:p2:p3:xs) | direction p2 p1 p == RIGHT ||
                                           direction p2 p1 p == STRAIGHT
                                             = graham_scan_adjust_stack p (p2:p3:xs)
                                         | direction p2 p1 p == RETURN
                                             = p1:p2:p3:xs
                                         | otherwise = p:p1:p2:p3:xs
graham_scan_adjust_stack p xs = p:xs

grahamConvexHull :: [Point] -> [Point]
grahamConvexHull setOfPoints =  let (startPoint : zs)     =  sortelemTupleList smallerelem setOfPoints
                                    pointList      =  sortelemTupleList ( smallerangle startPoint ) zs
                                    step xs p = graham_scan_adjust_stack p xs
                                in  foldl' step [startPoint] pointList
                                   
                                        

asIntFold :: String -> Int
asIntFold "" = error "Not good"
asIntFold ('-' : xc) = -1 * asIntFold xc
asIntFold s = foldl' step 0 s
                where step x y | x == 0  || signum s' == signum x = s'
                               | otherwise = error "Overflow"
                        where s' = if isHexDigit y then x * 10 + digitToInt y
                                                   else error "Fuck, no digit" 

type ErrorMessage = String
asIntFoldEither :: String -> Either ErrorMessage Int

asIntFoldEither "" = Left "Not good"

asIntFoldEither ('-' : xc) = case asIntFoldEither xc of
                                  Right x -> Right (-1 * x)
                                  Left  x -> Left x

--asIntFoldEither s = foldl' step (Right 0) s
--                       where step x' y = if isHexDigit y then 
--                                                          case x' of
--                                                               Right x -> if  x < 53687091 + digitToInt y 
--                                                                                        then Right ( x * 10 + digitToInt y )
--                                                                                        else Left "Overflow"
--                                                                                             
--                                                                                                           
--                                                               Left x -> Left x
--                                                         else Left "Oh my god, no digt"
asIntFoldEither s = foldl' step (Right 0) s
                       where step x' y = case x' of
                                         Left  _              -> x'
                                         Right r              -> if isHexDigit y
                                                                 then if r < 53687091 + digitToInt y
                                                                      then Right ( r * 10 + digitToInt y )
                                                                      else Left "Overflow"
                                                                 else Left "Shit, no digit"    

grahamTestSample = [(0.3215348546593775,0.03629583077160248),
                      (0.02402358131857918,-0.2356728797179394),
                      (0.04590851212470659,-0.4156409924995536),
                      (0.3218384001607433,0.1379850698988746),
                      (0.11506479756447,-0.1059521474930943),
                      (0.2622539999543261,-0.29702873322836),
                      (-0.161920957418085,-0.4055339716426413),
                      (0.1905378631228002,0.3698601009043493),
                      (0.2387090918968516,-0.01629827079949742),
                      (0.07495888748668034,-0.1659825110491202),
                      (0.3319341836794598,-0.1821814101954749),
                      (0.07703635755650362,-0.2499430638271785),
                      (0.2069242999022122,-0.2232970760420869),
                      (0.04604079532068295,-0.1923573186549892),
                      (0.05054295812784038,0.4754929463150845),
                      (-0.3900589168910486,0.2797829520700341),
                      (0.3120693385713448,-0.0506329867529059),
                      (0.01138812723698857,0.4002504701728471),
                      (0.009645149586391732,0.1060251100976254),
                      (-0.03597933197019559,0.2953639456959105),
                      (0.1818290866742182,0.001454397571696298),
                      (0.444056063372694,0.2502497166863175),
                      (-0.05301752458607545,-0.06553921621808712),
                      (0.4823896228171788,-0.4776170002088109),
                      (-0.3089226845734964,-0.06356112199235814),
                      (-0.271780741188471,0.1810810595574612),
                      (0.4293626522918815,0.2980897964891882),
                      (-0.004796652127799228,0.382663812844701),
                      (0.430695573269106,-0.2995073500084759),
                      (0.1799668387323309,-0.2973467472915973),
                      (0.4932166845474547,0.4928094162538735),
                      (-0.3521487911717489,0.4352656197131292),
                      (-0.4907368011686362,0.1865826865533206),
                      (-0.1047924716070224,-0.247073392148198),
                      (0.4374961861758457,-0.001606279519951237),
                      (0.003256207800708899,-0.2729194320486108),
                      (0.04310378203457577,0.4452604050238248),
                      (0.4916198379282093,-0.345391701297268),
                      (0.001675087028811806,0.1531837672490476),
                      (-0.4404289572876217,-0.2894855991839297)]

grahamTestSampleResult = [(-0.161920957418085,-0.4055339716426413),
                             (0.05054295812784038,0.4754929463150845),
                             (0.4823896228171788,-0.4776170002088109),
                             (0.4932166845474547,0.4928094162538735),
                             (-0.3521487911717489,0.4352656197131292),
                             (-0.4907368011686362,0.1865826865533206),
                             (0.4916198379282093,-0.345391701297268),
                             (-0.4404289572876217,-0.2894855991839297)]
