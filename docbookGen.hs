import System.Environment
import EqManips.DocBookGenerator

main :: IO ()
main = do
	args <- getArgs
	let filename = if null args then "eqDocBook.xml"
								else args !! 0
	writeFile filename $ generateDocBook ""

