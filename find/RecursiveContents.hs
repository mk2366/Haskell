{-# LANGUAGE ScopedTypeVariables #-}
module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>))
import Control.Exception

getRecursiveContents :: FilePath  -> IO [FilePath]

getRecursiveContents topdir = do
    names <- catch (getDirectoryContents topdir) (\(ex :: SomeException) -> return [".", ".."])
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- catch (doesDirectoryExist path) (\(ex :: SomeException) -> return False)
        if isDirectory then
                          getRecursiveContents path
                       else
                          return [path]
    return (concat paths)
