module Main where

import Control.Monad
import Data.Functor
import Download (downloadBook)
import Fmt hiding (format)
import Formatters
import qualified Interactive
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

processArgs :: [String] -> IO ()
processArgs ["-i"] = Interactive.run
processArgs ["--interactive"] = Interactive.run
processArgs ["--download", url, filename] = downloadBook url filename
processArgs ["-d", url, filename] = downloadBook url filename
processArgs [query] = getBooks query >>= printBooks Table
processArgs [query, "--yaml"] = getBooks query >>= printBooks YAML
processArgs _ = usage

main :: IO ()
main = getArgs >>= processArgs
