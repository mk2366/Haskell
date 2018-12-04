main = do
    putStrLn "Name?"
    name <- getLine
    putStrLn $ "Hi " ++ name
