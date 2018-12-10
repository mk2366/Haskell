module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception
import Control.Monad(forM)
import GlobRegex (matchesGlob)

namesMatching pat | not (isPattern pat) = doesNameExist pat >>= return (\a -> [pat | a])
                  | otherwise = 
                                case splitFileName pat of
                                 ("", baseName) -> do
                                     curDir <- getCurrentDirectory
                                     listMatches curDir baseName
                                 (dirName, baseName) -> do
                                     dirs <- if isPattern dirName then namesMatching (dropTrailingPathSeparator dirName)
                                                                  else return [dirName]
                                     let listDir  = if isPattern baseName then listMatches
                                                                          else listPlain
                                     pathNames <- forM dirs $ \dir -> do
                                                                 baseNames <- listDir dir baseName
                                                                 return (map (dir </>) basenames)
                                     return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists then return True
                  else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName then getCurrentDirectory
                                else return dirName
    handle (const (return [])) $ do
        names <- getDirectoryContents dirname'
        let names' = if isHidden pat then filter isHidden names
                                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False
                                                                        
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")