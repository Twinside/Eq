module Language.Eq.Renderer.Sexpr( sexprRender, sexprRenderS ) where

import Data.Ratio
import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.Algorithm.Utils

sexprRender :: Formula anyForm -> String
sexprRender f = sexprRenderS f ""

sexprRenderS :: Formula anyForm -> ShowS
sexprRenderS (Formula f) = sexprS f

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

sexprS :: FormulaPrim -> ShowS
sexprS (Complex _ (re, im)) = str "(complex " . sexprS re . char ' ' . sexprS im . char ')'
sexprS (Fraction f) = str"(% " . shows (numerator f) 
                               . str " " 
                               . shows (denominator f)
                               . str ") "
sexprS (Poly _ v@(PolyRest _)) = sexprS . unTagFormula $ convertToFormula v
sexprS (Poly _ (Polynome v lst)) =
    str "(poly " . str v . char ' ' . concatMapS coeffPrinter lst . char ')'
        where coeffSexpr = sexprS . unTagFormula . convertToFormula . PolyRest
              coeffPrinter (coeff, polyn) =
                    char '(' . coeffSexpr coeff . str ", "
                  . sexprS (poly polyn)
                  . str ") "

sexprS (List _ lst) =
    str "(list " . concatMapS (\a -> char ' ' . sexprS a) lst . str ") "
sexprS (Stack _ lst) =
    str "(stack " . concatMapS (\a -> char ' ' . sexprS a) lst . str ") "
sexprS (Display _ lst) =
    str "(display " . concatMapS (\a -> char ' ' . sexprS a) lst . str ") "

sexprS (Infer _ l1 l2) =
    str "(infer [" . premices . str "] (" . rez . str "))"
        where toL = concatMapS (\a -> char ' ' . sexprS a) 
              rez = toL l2
              premices = concatMapS toL l1

sexprS (Indexes _ main lst) =
    str "(indexes " . sexprS main . char ' ' 
                    . concatMapS (\a -> char ' ' . sexprS a) lst . str ") "

sexprS (Block _ _ _) = str "(block)"
sexprS Void = str "(void)"
sexprS (Variable v) = str v
sexprS (NumEntity e) = shows e
sexprS (Truth t) = shows t
sexprS (CInteger i) = shows i
sexprS (CFloat d) = shows d
sexprS (Meta _ op f) = char '(' . shows op . char ' ' . sexprS f . char ')'
sexprS (Lambda _ clauses) =
    str "(lambda " . concatMapS clauseRender clauses
                   . char ')'
        where clauseRender (args, body) =
                  str "((" . interspereseS (' ':) (map sexprS args) . str ") "
                           . sexprS body
                           . char ')'

sexprS (BinOp _ op lst) =
    char '(' . str (binopString op)
             . concatMapS (\a -> char ' ' . sexprS a) lst
             . char ')'

sexprS (UnOp _ op f) = char '(' . str (unopString op) . char ' '
                                . sexprS f . char ')'

sexprS (Sum _ begin end what) =
    str "(sum " . sexprS begin . char ' '
                . sexprS end . char ' '
                . sexprS what . char ')'

sexprS (Product _ begin end what) =
    str "(product " . sexprS begin . char ' '
                    . sexprS end . char ' '
                    . sexprS what . char ')'

sexprS (Integrate _ begin end what var) =
    str "(integral " . sexprS begin . char ' '
                     . sexprS end . char ' '
                     . sexprS what . char ' '
                     . sexprS var . char ')'

sexprS (Derivate _ f var) =
    str "(derivate " . sexprS f . char ' '
                     . sexprS var . char ')'

sexprS (App _ func args) = 
    str "(apply " . sexprS func . char ' '
                  . interspereseS (' ':) (map sexprS args)
                  . char ')'

sexprS (Matrix _ n m lsts) =
    str "(matrix " . shows n . char ' ' . shows m . char ' '
                   . concatS [concatMapS (\a -> (' ':) . sexprS a) lst | lst <- lsts]
                   . char ')'

