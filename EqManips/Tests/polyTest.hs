import EqManips.Types
import EqManips.Algorithm.Polynome
import EqManips.Algorithm.Utils

main :: IO ()
main = do
    --let formulaText = "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4 + 1"
    --let formulaText = "3 * x^2 * y^2 - (1/2) * x^2 * y * z^3 +5.0 * x^2 * z^2 +4*x -z^4"
    --let formulaText = "(3*y^2+(-2*z^3)*y+5*z^2)*x^2+4*x+(-z^4+1)"
    --let formulaText = "(-2*y*z^3)"
    --let formulaText = "(2*z^3)"
    let formulaText = "(-2*(-y)*z^3)"
    {-let formulaText = "((-(-y))*z^3)"-}
    either print (\formula -> do
            putStr "\n==========================================\n"
            putStr $ show formula
            putStr "\n==========================================\n"
            putStrLn . show . convertToPolynome $ formula)
         $ parseFormula formulaText

