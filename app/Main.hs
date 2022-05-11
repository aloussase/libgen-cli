module Main where

import Control.Monad
import Data.Functor
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
        "   libgen-cli [--help] [--yaml] [-d,--download <url>] [query]\n\n",
        "Flags:\n",
        "   --help          show this message and exit\n",
        "   --yaml          print the results as YAML\n",
        "   -d,--download   download the book\n"
      ]

printBooks :: (BookFormatter f) => f -> Maybe [Book] -> IO ()
printBooks _ Nothing = pure ()
printBooks fmt (Just books) = forM_ books (putStrLn . (format fmt))

processArgs :: [String] -> IO ()
processArgs [query] = getBooks query >>= (printBooks Table)
processArgs [query, "--yaml"] = getBooks query >>= (printBooks YAML)
processArgs ["--download", url] = undefined
processArgs ["-d", url] = undefined
processArgs _ = usage

main :: IO ()
main = getArgs >>= processArgs
