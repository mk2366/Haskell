{-# LANGUAGE OverloadedStrings #-}

--import qualified Data.ByteString.Lazy.Char8 as L8
--import           Network.HTTP.Client
import Network.Wreq
import Text.Feed.Import
import Text.Feed.Export
import Control.Lens
import Data.Text.Lazy

main = do
--    manager <- newManager defaultManagerSettings
--    request <- parseRequest "http://www.spiegel.de/schlagzeilen/tops/index.rss"
--    response <- httpLbs request manager
    response <- get "http://www.spiegel.de/schlagzeilen/tops/index.rss"
    let body = response ^. responseBody
    let (Just feed) = parseFeedSource body
    let (Just textfeed) = textFeed feed
    let ppfeed = unpack textfeed
    putStrLn $ ppfeed
--    print (body)