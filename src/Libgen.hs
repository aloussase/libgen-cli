{-# LANGUAGE OverloadedStrings #-}

module Libgen
    ( getBooks
    , Book(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Fmt
import           Network.URL                    ( encString
                                                , ok_url
                                                )
import           Text.HTML.Scalpel
import           Text.Read
import           Text.Regex

data Book = Book
    { bookUrl       :: URL
    , bookAuthors   :: [String]
    , bookTitle     :: String
    , bookYear      :: Int
    , bookExtension :: String
    , bookSize      :: String
    }
    deriving Show

type Query = String

getURL :: Query -> URL
getURL query = encString
    True
    ok_url
    ("http://libgen.is/search.php?req=" <> query <> "&res=25")

getBooks :: Query -> IO (Maybe [Book])
getBooks query = scrapeURL (getURL query) books

books = chroots ("tr" @: ["valign" @= "top"]) book

book :: Scraper String Book
book = Book <$> url <*> authors <*> title <*> year <*> extension <*> size

dummy :: a -> Scraper String a
dummy = pure

year :: Scraper String Int
year = do
    year' <- text $ "td" @: ["nowrap" @= ""]
    if isInteger year' then return $ read year' else empty

url :: Scraper String URL
url = attr "href" $ "a" @: ["title" @= "this mirror"]

authors :: Scraper String [String]
authors = texts $ "a" @: ["href" @=~ mkRegex ".*column=author"]

title :: Scraper String String
title = text $ "a" @: [match bookInfo]

size :: Scraper String String
size = do
    szs <- texts $ "td" @: ["nowrap" @= ""]
    case filter isSizeField szs of
        []      -> empty
        (x : _) -> return x

extensions :: [String]
extensions = ["pdf", "mobi", "epub", "chm"]

extension :: Scraper String String
extension = do
    extensions' <- texts $ "td" @: ["nowrap" @= ""]
    case filter (`elem` extensions) extensions' of
        []      -> empty
        (x : _) -> return x

isSizeField :: String -> Bool
isSizeField s = case words s of
    [year         ] -> False
    (size : format) -> isInteger size
    _               -> False

isInteger :: String -> Bool
isInteger n = isJust (readMaybe n :: Maybe Int)

bookInfo :: String -> String -> Bool
bookInfo "id" val = isInteger val
bookInfo _    _   = False
