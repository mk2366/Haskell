import Control.Monad.Trans.State
import System.Random
import Control.Monad

rollDie :: State StdGen Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

insights :: State StdGen StdGen
insights = get

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

getRandom :: Random a => State StdGen a
getRandom = state random

allTypes :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypes = (,,,,,,) <$> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom
                    <*> getRandom