module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception
import Control.Monad(forM)
import GlobRegex (matchesGlob)

namesMatching :: FilePath -> IO [String]
namesMatching pat | not (isPattern pat) = doesNameExist pat >>= (\a -> return [pat | a])
                  | otherwise = 
                                case splitFileName pat of
                                 ("", baseName) -> 
                                     getCurrentDirectory >>=
                                     (`listMatches` baseName)
                                 (dirName, baseName) -> 
                                    let listDir  = if isPattern baseName then listMatches
                                                                         else listPlain
                                        in
                                         concat <$>                     
                                         ((if isPattern dirName then namesMatching (dropTrailingPathSeparator dirName)
                                                               else return [dirName])
                                         >>=
                                         (\dirs -> forM dirs $ \dir -> doesFileExist dir >>= (\b -> if not b then
                                                                 map (dir </>) <$>
                                                                 listDir dir baseName 
                                                                                          else
                                                                 return [])))

doesNameExist :: FilePath -> IO Bool
doesNameExist name = doesFileExist name >>= (\fileExists ->
    if fileExists then return True
                  else doesDirectoryExist name)

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = 
    filter (`matchesGlob` pat) <$>
    ((if null dirName then getCurrentDirectory
                     else return dirName)
        >>=
        handle (\(SomeException e) -> return []) . getDirectoryContents 
        >>= (\names -> if isHidden pat then return (filter isHidden names)
                                       else return (filter (not . isHidden) names)))

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName then doesDirectoryExist dirName
                               else doesNameExist (dirName </> baseName)
    return [baseName | exists]

isHidden ('.':_) = True
isHidden _       = False
                                                                        
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")