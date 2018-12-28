{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, IOException(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(ex :: IOException) -> return Nothing) $
    withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(ex :: IOException) -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h 
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
                       where 
                        check name = do
                            perms <- getPermissions name
                            size  <- saferFileSize name
                            modified <- getModificationTime name
                            return (p name perms size modified)

