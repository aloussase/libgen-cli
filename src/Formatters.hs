{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatters where

import Data.List
import Fmt
import Libgen

class BookFormatter a where
  format :: a -> Book -> String

data YAML = YAML

instance BookFormatter YAML where
  format _ Book {bookUrl, bookAuthors, bookTitle, bookYear, bookExtension, bookSize} =
    fmt $
      mconcat
        [ nameF "title" (bookTitle |+ "\n"),
          nameF "url" (bookUrl |+ "\n"),
          nameF "year" (bookYear |+ "\n"),
          nameF "ext" (bookExtension |+ "\n"),
          nameF "size" (bookSize |+ "\n"),
          nameF "authors" $ indentF 4 (blockListF bookAuthors)
        ]

data Table = Table

getBookAuthor [] = "N/A"
getBookAuthor xs = intercalate ", " xs

instance BookFormatter Table where
  format _ Book {bookUrl, bookAuthors, bookTitle, bookYear, bookExtension, bookSize} =
    fmt $
      mconcat
        [ build bookUrl,
          "|" +| build bookTitle,
          "|" +| build (getBookAuthor bookAuthors),
          "|" +| build bookYear,
          "|" +| build bookExtension,
          "|" +| build bookSize
        ]
