
import EqManips.Types
import EqManips.Algorithm.Eval
import EqManips.Algorithm.Utils
import EqManips.EvaluationContext
import System.IO

escQuote :: Char -> [Char]
escQuote '"' = ['\\', '"']
escQuote a = [a]

-- | This script parse base-library.txt and feed a symbol
-- table. This symbol table is dumped and outputed to an
-- haskell file for further use
main :: IO ()
main = do
	eqLib <- readFile "EqManips/base-library.txt"
	let formulaList = parseProgramm eqLib
	either (error "Error") (\formulal ->
	    let rez = performLastTransformation $
						mapM evalGlobalLosslessStatement formulal
		in do
		outFile <- openFile "EqManips\\BaseLibrary.hs" WriteMode
		hPutStr outFile "module EqManips.BaseLibrary( defaultSymbolTable ) where\n"
		hPutStr outFile "\n"
		hPutStr outFile "import EqManips.Types\n"
		hPutStr outFile "import Data.Map\n"
		hPutStr outFile "\n"
		hPutStr outFile "defaultSymbolTable :: Map String Formula\n"
		hPutStr outFile "defaultSymbolTable = read \""

		hPutStr outFile . concatMap escQuote . show $ context rez

		hPutStr outFile "\"\n\n"
		hClose outFile
	  ) formulaList



