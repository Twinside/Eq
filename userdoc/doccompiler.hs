import Control.Applicative
import Control.Monad( when )

import Control.Monad.State.Lazy

import System.Process
import System.Environment
import System.Exit

import Text.Regex.TDFA

type Match = Maybe (String,String,String)
type MatchExtended = Maybe (String,String,String, [String])

codeBrush, outputBrush :: String
codeBrush = "brush: shell"
outputBrush = "brush: js"

printUsage :: IO ()
printUsage = print "bidule [infile] [outfile]"

parseParam :: String -> [String]
parseParam [] = []
parseParam (' ':xs) = parseParam xs
parseParam ('\t':xs) = parseParam xs
parseParam s = maybe [] dumper $ (s =~~ "([^ ]*)|(\"[^\"]*\")" :: Match)
    where unquote = filter (/= '"')
          dumper a@(_, [], _) = [show a]
          dumper a@(_, ma, after) = unquote ma : parseParam after

commandMatcher :: String -> StateT String IO (Maybe [String])
commandMatcher str = 
    case (str =~~ "<!-- %% (.*) -->" :: MatchExtended) of
         Nothing -> return Nothing
         Just (_,_,_, [command]) -> 
            let (prog:params) = parseParam command
            in do
             text <- liftIO $ readProcess prog params ""
             put text
             return $ Just ["<pre class=\"" ++ codeBrush ++ "\">", command, "</pre>"]

         Just _ -> return Nothing

insertMatcher :: String -> StateT String IO (Maybe [String])
insertMatcher str = case (str =~~ "<!-- %%%([^ ]*) -->" :: MatchExtended) of
        Nothing -> return Nothing
        Just (prev,curr,_, []) -> do
            toInsert <- get
            return $ Just [prev, curr
                          ,"<pre class=\"" ++ outputBrush ++ "\">", toInsert, "</pre>"]
        Just (prev,curr,_, [kind]) -> do
            toInsert <- get
            return $ Just [prev, curr
                          ,"<pre class=\"brush: " ++ kind ++ "\">", toInsert, "</pre>"]

includerMatcher :: String -> StateT String IO (Maybe [String])
includerMatcher str = case (str =~~ ".*<!-- %INCLUDE% ([^ ]*) -->" :: MatchExtended) of
        Nothing -> return Nothing
        Just (_,_,_,[file]) -> do
            fileData <- liftIO $ readFile file
            return $ Just [fileData]

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
    

