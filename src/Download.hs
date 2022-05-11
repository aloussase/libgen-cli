{-# LANGUAGE OverloadedStrings #-}

module Download where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Functor
import Libgen
import Network.Curl.Download
import Text.HTML.Scalpel

download :: URL -> FilePath -> IO (Either String ())
download bookUrl filepath = do
  url <- scrapeURL bookUrl getDownloadUrl
  case url of
    Just (Just url') -> do
      putStrLn $ "Saving book to " <> filepath
      openURI url' >>= saveFile filepath
    _ -> pure $ Left "Failed to download book"

saveFile :: String -> Either String B.ByteString -> IO (Either String ())
saveFile title (Right contents) = B.writeFile title contents <&> Right
saveFile _ (Left err) = pure $ Left err

getDownloadUrl :: Scraper String (Maybe URL)
getDownloadUrl = do
  links <- chroots "ul" cloudflareLink
  case links of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

cloudflareLink :: Scraper String URL
cloudflareLink = do
  aText <- text "a"
  aLink <- attr "href" "a"

  if aText == "Cloudflare"
    then return aLink
    else empty
