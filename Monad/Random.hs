import System.Random
import Control.Monad.State.Lazy

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = 
    get >>= \gen ->
        let (val, gen') = random gen
        in
            put gen' >>
            return val

get2Randoms :: (Random a, Random b) => RandomState (a,b)
get2Randoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Bool, Int)
runTwoRandoms = do
    oldState <- getStdGen
    let (result, newState) = runState get2Randoms oldState
    setStdGen newState
    return result