type PersonSchuh = (String, Integer)
type PersonPerson = (String, String)

aFeetSize :: [PersonSchuh]
aFeetSize = [
  ("Kim00", 42),
  ("Kim01", 41),
  ("Kim02", 39),
  ("Kim03", 44),
  ("Kim04", 39),
  ("Kim05", 41),
  ("Kim06", 40),
  ("Kim07", 45),
  ("Kim08", 42),
  ("Kim09", 41),
  ("Kim10", 43),
  ("Kim11", 42)]

 
aBootsGiven = [
  ("Kim00", 40),
  ("Kim01", 41),
  ("Kim02", 42),
  ("Kim03", 39),
  ("Kim04", 41),
  ("Kim05", 45),
  ("Kim06", 39),
  ("Kim07", 44),
  ("Kim08", 41),
  ("Kim09", 43),
  ("Kim10", 42),
  ("Kim11", 42)]

-- getBoots (aFeetSize, aBootsGiven) = mapSnd sortBy (\(a,b) (c,d) -> b) reduceFitting (aFeetSize, aBootsGiven)


reduceFitting ((a1:aFeetSize),(a2:aBootsGiven)) | a1 == a2  = reduceFitting (aFeetSize, aBootsGiven)
                                           | otherwise = add_lists a1 a2 $ reduceFitting (aFeetSize, aBootsGiven)
                                             where add_lists a b (l1,l2) = (a:l1,b:l2)
reduceFitting _ = ([],[])