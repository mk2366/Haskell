import System.IO
import System.Directory(getTemporaryDirectory,removeFile)
import Control.Exception(catch, finally, IOException)

main :: IO ()
main = withTempFile "markus.txt" myAction


myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = do 
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temp file at: " ++ tempname
    pos <- hTell temph
    putStrLn $ "Initial Position is; " ++ show pos
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes"
    hPutStrLn temph tempdata
    pos <- hTell temph
    putStrLn $ "after writing my new position is: " ++ show pos
    putStrLn $ "The content of the file is: "
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph
    putStrLn c 
    putStrLn $ "which corresponds the Haskell literal:"
    print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
    tempdir <- catch (getTemporaryDirectory) (\e -> do
        let _ = e :: IOException
        return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph) (do hClose temph
                                      removeFile tempfile
                                      )