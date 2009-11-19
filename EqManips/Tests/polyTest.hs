import Control.Applicative( (<$>) )
import Data.Maybe( fromMaybe )
import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils
import EqManips.InputParser.EqCode
import EqManips.Renderer.Sexpr

polynomes :: [String]
polynomes =
        [ "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4 + 1"
        , "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4"
        , "(3*y^2+(-2*z^3)*y+5*z^2)*x^2+4*x+(-z^4+1)"
        , "(3*y^2+(-2*z^3)*y+5*z^2)*x^2+4*x+(-z^4+1)"
        , "(-2*y*z^3)"
        , "(2*z^3)"
        , "(-2*(-y)*z^3)"
        , "((-(-y))*z^3)"
        , "x^2*y^2 + 5 + y^5 + y^3 + x*y + x"
        , "x*y + x"
        ]

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
            let rez = convertToPolynome formula
                nice = sexprRender . Formula . Poly <$> rez
            putStrLn $ fromMaybe "Nothing" nice
            putStr "\n==========================================\n"
            )
         $ either Left (Right) . perfectParse $ formulaText

main :: IO ()
main = do
    {-formulaPolynomiezer $ polynomes !! 2-}
    {-formulaPolynomiezer $ polynomes !! 3-}
    mapM_ formulaPolynomiezer polynomes

