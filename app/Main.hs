module Main where

import Control.Monad
import Data.Functor
import Download (download)
import Fmt hiding (format)
import Formatters
import Libgen
import System.Environment
import System.IO
import Text.Read (readMaybe)

usage :: IO ()
usage =
  putStrLn $
    mconcat
      [ "Usage of libgen-cli:\n",
        "   libgen-cli [--help] [--yaml] [-d,--download <url> <filename>] [query]\n\n",
        "Flags:\n",
        "   -h, --help       show this message and exit\n",
        "   --yaml           print the results as YAML\n",
        "   -d, --download   download the book"
      ]

printBooks :: (BookFormatter f) => f -> Maybe [Book] -> IO ()
printBooks _ Nothing = pure ()
printBooks fmt (Just books) = forM_ books (putStrLn . format fmt)

downloadBook :: String -> String -> IO ()
downloadBook url filename = do
  result <- download url filename
  case result of
    Left err -> putStrLn err
    _ -> pure ()

processArgs :: [String] -> IO ()
processArgs [query] = getBooks query >>= printBooks Table
processArgs [query, "--yaml"] = getBooks query >>= printBooks YAML
processArgs ["--download", url, filename] = downloadBook url filename
processArgs ["-d", url, filename] = downloadBook url filename
processArgs _ = usage

main :: IO ()
main = getArgs >>= processArgs
