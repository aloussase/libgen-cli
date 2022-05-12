module Main where

import           Control.Monad
import           Data.Functor
import           Download                       ( download )
import           Fmt                     hiding ( format )
import           Formatters
import           Interactive                    ( runInteractiveApp )
import           Libgen
import           System.Environment
import           System.IO
import           Text.Read                      ( readMaybe )

usage :: IO ()
usage = putStrLn $ mconcat
    [ "Usage of libgen-cli:\n"
    , "   libgen-cli [-h|--help] [--yaml] [-d|--download <url> <filename>] [query]\n\n"
    , "Flags:\n"
    , "   -h, --help            show this message and exit\n"
    , "   --yaml                print the search results as YAML\n"
    , "   -d, --download        download the book found at a given url\n"
    , "   -i, --interactive     search for books interactively\n\n"
    , "Example:\n\n"
    , "Download a book called 'some book title' and save to a file of the same name:\n\n"
    , "   libgen-cli 'some book title' | awk -F'|' '$5 ~ /pdf/ {print \"$1 $2\"}' | xargs libgen-cli -d"
    ]

processArgs :: [String] -> IO ()
processArgs ["-i"           ] = runInteractiveApp
processArgs ["--interactive"] = runInteractiveApp

processArgs [command, url, filename]
    | command == "--download" || command == "-d" = do
        result <- download url filename
        case result of
            Left err -> putStrLn err
            _        -> putStrLn $ "Book was saved to " <> filename
    | otherwise = usage

processArgs [query]           = getBooks query >>= printBooks Table
processArgs [query, "--yaml"] = getBooks query >>= printBooks YAML

processArgs _                 = usage

main :: IO ()
main = getArgs >>= processArgs
