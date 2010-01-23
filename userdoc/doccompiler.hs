import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

import Data.Char

import System.Process
import System.Environment
import System.Exit

import Text.Regex.TDFA

-- Mumble, shell call
import System.Process.Internals

import System.IO.Error
import qualified Control.Exception as C
import Control.Concurrent
import Foreign
import Foreign.C
import System.IO
-- /Mumble

type Match = Maybe (String,String,String)
type MatchExtended = Maybe (String,String,String, [String])

codeBrush, outputBrush :: String
codeBrush = "brush: shell"
outputBrush = "brush: shell"

printUsage :: IO ()
printUsage = print "bidule [infile] [outfile]"

shellExecute :: FilePath -> IO String
shellExecute cmd = do
    (_ , Just outh, Just errh, pid) <-
        createProcess (shell cmd){ std_in  = Inherit,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    errOutput <- hGetContents errh
    outMVar <- newEmptyMVar
    errMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()
    forkIO $ C.evaluate (length errOutput) >> putMVar errMVar ()

    -- wait on the output
    takeMVar outMVar
    hClose outh

    takeMVar errMVar
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return $ errOutput ++ output
     ExitFailure r -> do
        putStrLn $ show r ++ " - " ++ cmd
        return ""

parseParam :: String -> [String]
parseParam [] = []
parseParam (' ':xs) = parseParam xs
parseParam ('\t':xs) = parseParam xs
parseParam s = maybe [] dumper (s =~~ "([^ ]*)|(\"[^\"]*\")" :: Match)
    where unquote = filter (/= '"')
          dumper a@(_, [], _) = [show a]
          dumper a@(_, ma, after) = unquote ma : parseParam after

commandMatcher :: String -> StateT String IO (Maybe [String])
commandMatcher str = 
    case (str =~~ "<!-- %% (.*) -->" :: MatchExtended) of
         Nothing -> return Nothing
         Just (_,_,_, [command]) -> 
            let realCommand = command --reverse . tail $ reverse command
            in do
             text <- liftIO $ shellExecute realCommand
             put text
             return $ Just ["<pre class=\"" ++ codeBrush ++ "\">", command, "</pre>"]

         Just _ -> return Nothing

insertMatcher :: String -> StateT String IO (Maybe [String])
insertMatcher str = produce (str =~~ "<!-- %%%([^ ]*) -->" :: MatchExtended)
    where produce Nothing = return Nothing
          produce (Just (prev,curr,_, [])) = do
            toInsert <- get
            return $ Just [prev, curr
                          ,"<pre class=\"" ++ outputBrush ++ "\">", toInsert, "</pre>"]
          produce (Just (prev,curr,next,[kind]))
                | all isSpace kind = produce (Just (prev, curr, next, []))
                | otherwise = do
                    toInsert <- get
                    return $ Just [prev, curr
                                  ,"<pre class=\"brush: " ++ kind ++ "\">", toInsert, "</pre>"]

includerMatcher :: String -> StateT String IO (Maybe [String])
includerMatcher str = case (str =~~ ".*<!-- %INCLUDE% ([^ ]*) -->" :: MatchExtended) of
        Nothing -> return Nothing
        Just (_,_,_,[file]) -> do
            fileData <- liftIO $ lines <$> readFile file
            Just . concat <$> mapM matcher fileData

preprocessCommand :: [String -> StateT String IO (Maybe [String])]
preprocessCommand = [ commandMatcher
                    , insertMatcher
                    , includerMatcher
                    ]

matcher :: String -> StateT String IO [String]
matcher line = applicator preprocessCommand
    where applicator [] = return [line]
          applicator (processor:xs) =
                processor line >>= maybe (applicator xs) return 

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 2)
         (do print "Wrong arguments"
             printUsage
             exitFailure)

    file <- lines <$> readFile (head args)
    (processed,_) <- runStateT (mapM matcher file) ""
    writeFile (args !! 1) . filter (/= '\r') . unlines $ concat processed
    

