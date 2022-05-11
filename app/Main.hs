module Main where

import Control.Monad
import Data.Functor
import Fmt
import Libgen
import System.Environment
import System.IO

banner :: String
banner = "Welcome to Libgen CLI!\n"

prompt :: String -> IO Query
prompt p = putStr p >> hFlush stdout >> getLine

printBooks :: Maybe [Book] -> IO ()
printBooks (Just books) = forM_ books (fmtLn . build)
printBooks Nothing = pure ()

main :: IO ()
main = do
  putStrLn banner
  query <- prompt "? "
  books <- getBooks query
  printBooks books
  pure ()
