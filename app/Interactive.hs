module Interactive
    ( runInteractiveApp
    ) where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.State
import           Download                       ( download )
import           Formatters
import           Libgen                         ( Book(..)
                                                , getBooks
                                                )
import           System.Console.ANSI
import           System.IO


-- | AppState - The state of the application consists of a list of books and an
-- an integer signaling the position of the current book in the list.
data AppState = AppState
    { currentBook :: Int
    , books       :: [Book]
    }

initialState :: AppState
initialState = AppState { currentBook = -1, books = [] }

type InteractiveApp = StateT AppState IO ()

runInteractiveApp :: IO ()
runInteractiveApp = evalStateT run initialState

commandMap :: [(String, [String] -> InteractiveApp)]
commandMap =
    [ ("h", const help)
    , ("s", search)
    , ("d", downloadBook)
    , ("n", const nextBook)
    , ("p", const prevBook)
    ]

-- | Welcome banner.
banner :: String
banner = "Welcome to libgen-cli! Press 'h' to see available commands"

-- | Utility functions for output.

msgsPosition :: (Int, Int)
msgsPosition = (2, 0)

outputPosition :: (Int, Int)
outputPosition = (4, 0)

gotoMsgs :: IO ()
gotoMsgs = uncurry setCursorPosition msgsPosition

gotoOutput :: IO ()
gotoOutput = uncurry setCursorPosition outputPosition

gotoOutputM :: InteractiveApp
gotoOutputM = liftIO gotoOutput

data MsgType = Success | Error | Processing

printMsgPrefix :: MsgType -> IO ()
printMsgPrefix Success =
    setSGR [SetColor Foreground Vivid Green] >> putStr "  \xFAE0 " >> setSGR []
printMsgPrefix Error =
    setSGR [SetColor Foreground Vivid Red] >> putStr "  \xF659 " >> setSGR []
printMsgPrefix Processing =
    setSGR [SetColor Foreground Vivid Yellow, SetBlinkSpeed SlowBlink]
        >> putStr "  \xF252 "
        >> setSGR []

msg :: MsgType -> String -> InteractiveApp
msg level text =
    liftIO
        $  saveCursor
        >> gotoMsgs
        >> printMsgPrefix level
        >> putStrLn text
        >> restoreCursor

clearMsgs :: InteractiveApp
clearMsgs =
    liftIO $ saveCursor >> gotoMsgs >> clearFromCursorToLineEnd >> restoreCursor

clearOutput :: InteractiveApp
clearOutput = liftIO $ gotoOutput >> clearFromCursorToScreenEnd

-- | Commands


-- | Print help message.
help :: InteractiveApp
help = do
    liftIO
        $  putStrLn
               (mconcat
                   [ "Commands:\n"
                   , "  h                 :     Show this help message\n"
                   , "  n                 :     Show next book from current search\n"
                   , "  p                 :     Show previous book from current search\n"
                   , "  d <url> <dest>    :     Download and save book the current book to <dest>\n"
                   , "  s <query>         :     List books matching <query>\n"
                   ]
               )
        >> putStrLn "Press any key to continue"
        >> getChar
    clearOutput

-- | Look for a book given a query and, if successful, modify the state with
-- the new list of books.
search :: [String] -> InteractiveApp
search []   = msg Error "Missing argument to search command: <query>"
search args = do
    books <- liftIO $ getBooks $ unwords args
    case books of
        Just books'@(x : _) ->
            modify (\s -> s { currentBook = 0, books = books' })
        _ -> msg Error "No books found :c"


-- | Update the state to point to the next book in the list of books.
nextBook :: InteractiveApp
nextBook = do
    n      <- gets currentBook
    nbooks <- gets (length . books)
    if n > -1
        then modify (\s -> s { currentBook = (n + 1) `rem` nbooks })
        else msg Error "There are no books!"

prevBook :: InteractiveApp
prevBook = do
    n      <- gets currentBook
    nbooks <- gets (length . books)
    case n of
        -1 -> msg Error "There are no books"
        0  -> modify (\s -> s { currentBook = nbooks - 1 })
        _  -> modify (\s -> s { currentBook = n - 1 })

-- | Download the current book to a file specified by @dest@.
downloadBook :: [String] -> InteractiveApp
downloadBook [dest] = do
    n     <- gets currentBook
    books <- gets books
    if n > -1
        then do
            msg Processing $ "Downloading book " <> bookTitle (books !! n)
            result <- liftIO $ download (bookUrl $ books !! n) dest
            case result of
                Left err -> msg Error err
                _        -> msg Success $ "Book was saved to " <> dest
        else msg Error "No book is currently selected"
downloadBook _ =
    msg Error "Invalid arguments for command download: expected <dest>"


-- | Show the current book to the user.
printCurrentBook :: InteractiveApp
printCurrentBook = do
    n <- gets currentBook
    when (n > -1) $ do
        books <- gets books
        liftIO
            $  gotoOutput
            >> putStrLn (format YAML (books !! n))
            >> putStrLn
                   ("Showing book " <> show (1 + n) <> " of " <> show
                       (length books)
                   )

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

run :: InteractiveApp
run = do
    liftIO
        $  hSetBuffering stdin NoBuffering
        >> setTitle "Libgen CLI"
        >> setCursorPosition 0 0
        >> clearScreen
        >> putStrLn banner
        >> gotoOutput
    loop
    liftIO $ clearScreen >> showCursor >> setCursorPosition 0 0
  where
    loop :: InteractiveApp
    loop = do
        printCurrentBook
        s <- liftIO $ words <$> prompt "? "
        case s of
            ("q"     : _   ) -> liftIO $ putStrLn "Bye-bye!"
            (command : args) -> runCommand command args >> loop
            _                -> clearMsgs >> clearOutput >> loop

    runCommand :: String -> [String] -> InteractiveApp
    runCommand command args = case lookup command commandMap of
        Just f -> clearMsgs >> clearOutput >> gotoOutputM >> f args
        Nothing ->
            clearOutput >> msg Error "Invalid command, press 'h' for help"
