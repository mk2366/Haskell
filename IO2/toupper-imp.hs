import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
    inh  <- openFile "basicio.hs" ReadMode
    outh <- openFile "out" ReadWriteMode
    hSeek outh SeekFromEnd 0
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
    ineof <- hIsEOF inh
    if ineof then
        return ()
    else
        do
            inStr <- hGetLine inh
            hPutStrLn outh (map toUpper inStr)
            mainloop inh outh