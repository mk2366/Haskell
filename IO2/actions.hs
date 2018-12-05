str2Action :: String -> IO ()
str2Action s = putStrLn ("Data:" ++ s)

list2Action :: [String] -> [IO ()]
list2Action = map str2Action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2Action strings

printall :: IO ()
printall = runall actions

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (a:as) = do a
                   runall as

action =  mapM (str2Action . show) numbers

getString :: IO String
getString = readLn

main :: IO [()]
main = action