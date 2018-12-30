{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, emptyPermissions)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, IOException(..),SomeException(..), catch)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)

import RecursiveContents (getRecursiveContents)

type Predicate = InfoP Bool

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

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

myTest2 = (liftPath takeExtension ==? ".hs") &&! (sizeP >? 200)

main = do
    list <- betterFind myTest2 "../.."
    putStrLn $ concat list