import Control.Applicative( (<$>) )
import Control.Monad( when )
import Data.Maybe( fromMaybe )
import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.Algorithm.Utils
import Language.Eq.InputParser.EqCode
import Language.Eq.Renderer.Sexpr

polynomes :: [String]
polynomes =
        [ "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4 + 1"
        , "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4"
        , "(3*y^2+(-2*z^3)*y+5*z^2)*x^2+4*x+(-z^4+1)"
        , "(-2*y*z^3)"
        , "(2*z^3)"
        , "(-2*(-y)*z^3)"
        , "((-(-y))*z^3)"
        , "x^2*y^2 + 5 + y^5 + y^3 + x*y + x"
        , "x*y + x"
        , "x^4 + x + x^5 - 3 + x"
        , "x^2 + x - 3"
        ]

isPolyFineRecursive :: Polynome -> Bool
isPolyFineRecursive (PolyRest _) = True
isPolyFineRecursive (Polynome x lst) = all (polyrec x) lst
    where polyrec _   (_,(PolyRest _)) = True
          polyrec var (_,(Polynome y lst'))
            | var < y = all (polyrec y) lst'
            | otherwise = False

isPolySorted :: Polynome -> Bool
isPolySorted (PolyRest _) = True
isPolySorted (Polynome x []) = True
isPolySorted (Polynome x [(_,sub)]) = isPolySorted sub
isPolySorted (Polynome x ((o1,sub):(o2,sub2):xs)) =
    o1 < o2 && isPolySorted sub && isPolySorted (Polynome x ((o2,sub2):xs))

formulaPolynomiezer :: String -> IO ()
formulaPolynomiezer formulaText =
    either print (\formula -> do
            putStr "\n==========================================\n"
            putStrLn formulaText
            putStr $ sexprRender formula
            let prepared = prepareFormula $ unTagFormula formula
            putStr "\n##########################################\n"
            putStr . sexprRender $ Formula prepared
            putStr "\n******************************************\n"

            case convertToPolynome formula of
                 Nothing -> putStrLn "Nothing"
                 Just p -> do
                     unless (isPolySorted p) $ putStrLn "/!\\ UNSORTED!!"
                     unless (isPolyFineRecursive p) $ putStrLn "/!\\ Unfine recursive"

                     putStrLn . sexprRender . Formula $ Poly p
            putStr "\n==========================================\n"
            )
         $ either Left Right . perfectParse $ formulaText

main :: IO ()
main = mapM_ formulaPolynomiezer polynomes

