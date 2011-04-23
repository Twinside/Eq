
import Language.Eq.Types
import Language.Eq.InputParser.EqCode
import Language.Eq.Algorithm.Eval
import Language.Eq.Algorithm.Utils
import Language.Eq.EvaluationContext
import System.IO
-- | Maybe replace this with template haskell...

-- | This script parse base-library.txt and feed a symbol
-- table. This symbol table is dumped and outputed to an
-- haskell file for further use
main :: IO ()
main = do
	eqLib <- readFile "Language/Eq/base-library.eq"
	let formulaList = parseProgramm eqLib
	either (error "Error") (\formulal ->
	    let rez = performLastTransformation $
						mapM evalGlobalLosslessStatement formulal
		in do
		outFile <- openFile "Language/Eq/BaseLibrary.hs" WriteMode
		hPutStr outFile "module Language.Eq.BaseLibrary( defaultSymbolTable ) where\n"
		hPutStr outFile "\n"
		hPutStr outFile "import Language.Eq.Types\n"
		hPutStr outFile "import Data.Map\n"
		hPutStr outFile "\n"
		hPutStr outFile "defaultSymbolTable :: Map String (Formula ListForm)\n"
		hPutStr outFile "defaultSymbolTable =  "

		hPutStr outFile . show $ context rez

		hPutStr outFile "\n\n"
		hClose outFile
	  ) formulaList



