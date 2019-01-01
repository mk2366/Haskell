import System.Random

threeCoins :: Int -> (Bool, Bool, Bool)  
threeCoins i =   
    let gen = mkStdGen i
        (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
