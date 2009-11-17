module EqManips.Renderer.Sexpr( sexprRender, sexprRenderS ) where

import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils

sexprRender :: Formula anyForm -> String
sexprRender f = sexprRenderS f ""

sexprRenderS :: Formula anyForm -> ShowS
sexprRenderS (Formula f) = sexprS f

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

sexprS :: FormulaPrim -> ShowS
sexprS (Poly v@(PolyRest _)) = sexprS . unTagFormula $ convertToFormula v
sexprS (Poly (Polynome v lst)) =
    str "(poly " . str v . char ' ' . concatMapS coeffPrinter lst . char ')'
        where coeffSexpr c = sexprS . unTagFormula $ convertToFormula (PolyRest c)
              coeffPrinter (coeff, poly) =
                    char '(' . coeffSexpr coeff . str ", "
                  . sexprS (Poly poly)
                  . str ") "

sexprS (Block _ _ _) = str "(block)"
sexprS (Variable v) = str v
sexprS (NumEntity e) = shows e
sexprS (Truth t) = shows t
sexprS (CInteger i) = shows i
sexprS (CFloat d) = shows d
sexprS (Meta op f) = char '(' . shows op . char ' ' . sexprS f . char ')'
sexprS (Lambda clauses) =
    str "(lambda " . concatMapS clauseRender clauses
                   . char ')'
        where clauseRender (args, body) =
                  str "((" . concatMapS sexprS args . str ") "
                           . sexprS body
                           . char ')'

sexprS (BinOp op lst) =
    char '(' . str (binopString op)
             . concatMapS (\a -> char ' ' . sexprS a) lst
             . char ')'

sexprS (UnOp op f) = char '(' . str (unopString op) . char ' '
                              . sexprS f . char ')'

sexprS (Sum begin end what) =
    str "(sum " . sexprS begin . char ' '
                . sexprS end . char ' '
                . sexprS what . char ')'

sexprS (Product begin end what) =
    str "(product " . sexprS begin . char ' '
                    . sexprS end . char ' '
                    . sexprS what . char ')'

sexprS (Integrate begin end what var) =
    str "(integral " . sexprS begin . char ' '
                     . sexprS end . char ' '
                     . sexprS what . char ' '
                     . sexprS var . char ')'

sexprS (Derivate f var) =
    str "(derivate " . sexprS f . char ' '
                     . sexprS var . char ')'

sexprS (App func args) = 
    str "(apply " . sexprS func
                  . concatMapS sexprS args
                  . char ')'

sexprS (Matrix n m lsts) =
    str "(matrix " . shows n . char ' ' . shows m
                   . concatS [concatMapS sexprS lst | lst <- lsts]
                   . char ')'

