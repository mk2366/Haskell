import System.Environment(getArgs, getEnvironment)

action :: IO()
action = putStrLn "Hi, Markus, wie geht es dir?" >>
         getLine >>=
            (\line -> putStrLn $ "Das ist schon okay, dass du dich " ++ line ++ " fühlst")
         >> getArgs >>=
            (\largs -> putStrLn $ unlines largs)
         >> getEnvironment >>=
            (\largs -> putStrLn $ unlines (map show largs))
                     >> return ()

main = action