{-# LANGUAGE ScopedTypeVariables #-}

import BetterPredicate
import System.FilePath ((</>), takeExtension, takeFileName)
import Data.Char (toLower)


data Iterate seed =   Done {unwrap :: seed}
                    | Skip {unwrap :: seed}
                    | Continue {unwrap :: seed}
                      deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: forall a . Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
    where
        fold :: a -> FilePath -> IO (Iterate a)
        fold seed subpath = getUsefulContents subpath >>= walk seed
        walk :: a -> [FilePath] -> IO (Iterate a)
        walk seed (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _) -> return done
                Skip seed'    -> walk seed' names
                Continue seed' | isDirectory info -> do
                                                     next <- fold seed' path'
                                                     case next of
                                                         done@(Done _) -> return done
                                                         iSeed         -> walk (unwrap iSeed) names
                               | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)

atMostThreeHaskellFiles :: Iterator [FilePath]
atMostThreeHaskellFiles paths info 
                            | length paths == 3 = Done paths
                            | isDirectory info && takeFileName path == ".git" = Skip paths
                            | extension `elem` [".hs"] = Continue (path:paths)
                            | otherwise = Continue paths
                            where extension = map toLower (takeExtension path)
                                  path = infoPath info

countDirectories :: Iterator Integer
countDirectories count info = Continue (if isDirectory info then count + 1 else count)


