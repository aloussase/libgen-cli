module Interactive (run) where

import Control.Monad
import Download (downloadBook)
import Formatters
import Libgen (getBooks)
import System.IO

commandMap :: [(String, [String] -> IO ())]
commandMap =
  [ ("h", help),
    ("l", list),
    ("d", downloadBook')
  ]

banner :: String
banner = "Welcome to libgen-cli! Press 'h' to see available commands"

help :: [String] -> IO ()
help _ =
  putStrLn $
    mconcat
      [ "Commands:\n",
        "  h                 : Show this help message\n",
        "  n                 : Show results from the next page\n",
        "  d <url> <dest>    : Download book from <url> to <dest>\n",
        "  l <query>         : List books matching <query>\n"
      ]

list :: [String] -> IO ()
list = (getBooks >=> printBooks YAML) . unwords

downloadBook' :: [String] -> IO ()
downloadBook' [url, dest] = downloadBook url dest
downloadBook' _ = putStrLn "Invalid arguments for command download: expected <url> <dest>"

prompt :: String -> IO String
prompt p = do
  putStr p
  hFlush stdout
  getLine

run :: IO ()
run = do
  putStrLn banner
  loop
  where
    loop = do
      (command : args) <- words <$> prompt "? "
      if command == "q"
        then putStrLn "Bye-bye!"
        else case lookup command commandMap of
          Just f -> f args >> loop
          Nothing -> putStrLn "Invalid command, press 'h' for help" >> loop
