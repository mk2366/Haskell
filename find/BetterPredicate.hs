{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad (filterM, forM, liftM, return, (>>=))
import System.Directory (Permissions(..), searchable, getModificationTime, getPermissions, emptyPermissions, getDirectoryContents)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle, IOException(..),SomeException(..), catch)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)
import Prelude (Bool(..), FilePath, Maybe(..), Integer, Show, Eq, Ord, IO, undefined, concat,
                 map, mapM, ($), (&&), (||), (<), (>), (==), putStrLn, String(..), (/=),
                 filter, notElem, (.), maybe, id, reverse)
import Data.Sort (sort)

import RecursiveContents (getRecursiveContents)

type Predicate = InfoP Bool

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

data Info = Info {
             infoPath :: FilePath
            ,infoPerms :: Maybe Permissions
            ,infoSize :: Maybe Integer
            ,infoModTime :: Maybe UTCTime
} deriving (Show, Eq, Ord)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size  <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(ex :: IOException) -> return Nothing)(Just `liftM` act)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> 
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".",".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

pathP :: InfoP FilePath
pathP filePath _ _ _ = filePath

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

liftP :: (a -> b -> p) -> InfoP a -> b -> InfoP p
--liftP predicate infoPa x a b c d = infoPa a b c d `predicate` x
liftP predicate infoPa x = liftP2 predicate infoPa (\_ _ _ _ -> x)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 f a b w x y z = a w x y z `f` b w x y z

andP, orP, (&&!), (||!) :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP  = liftP2 (||)
(&&!) = andP
(||!) = orP

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

greaterP, lesserP, equalP, (>?), (<?), (==?) :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)
equalP   = liftP (==)
(>?) = greaterP
(<?) = lesserP
(==?) = equalP

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
                            perms <- catch (getPermissions name) (\(ex :: SomeException) -> return emptyPermissions)
                            size  <- getFileSize name
                            modified <- catch (getModificationTime name) (\(ex :: SomeException) -> getCurrentTime)
                            return (p name perms size modified)

betterFind2 :: ([Info] -> [Info]) -> FilePath -> Predicate -> IO [FilePath]
betterFind2 order path p = liftM (map infoPath) (traverse order path >>= filterM check)
                     where 
                        check Info{..} =  return (p infoPath infoPerms infoSize infoModTime)

myTest2 = (liftPath takeExtension ==? ".hs") &&! (sizeP >? 200)

main = do
    list <- betterFind myTest2 "../.."
    putStrLn $ concat list