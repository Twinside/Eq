module EqManips.Renderer.EqCode( unparse, unparseS ) where

import Data.List( foldl' )
import Data.Ratio

import EqManips.Types
import EqManips.Propreties
import EqManips.Polynome( convertToFormula )

-- | Public function to translate a formula back to it's
-- original notation. NOTE : it's not used as a Show instance...
unparse :: FormulaPrim -> String
unparse f = unparseS f ""

unparseS :: FormulaPrim -> ShowS
unparseS  = deparse maxPrio False

-- | used to render functions' arguments
argListToString :: [FormulaPrim] -> ShowS
argListToString [] = id
argListToString [f] = deparse maxPrio False f
argListToString lst = foldl' accum (unprint lastElem) reved
    where unprint = deparse maxPrio False
          accum acc f = unprint f . (',':) . acc
          (lastElem:reved) = reverse lst

-- | only to avoid a weird constant somewhere
maxPrio :: Int
maxPrio = 15

-- | Real conversion function, pass down priority
-- and tree direction
deparse :: Int -> Bool -> FormulaPrim -> ShowS
-- INVISIBLE META NINJA !!
deparse i r (Meta op f) = (++) (show op) . ('(' :) . deparse i r f . (')':)
deparse i r (Poly p) = deparse i r . unTagFormula $ convertToFormula p
deparse _ _ (Truth True) = ("true" ++)
deparse _ _ (Truth False) = ("false" ++)
deparse _ _ (BinOp _ []) =
    error "The formula is denormalized : a binary operator without any operands"
deparse _ _ (Variable s) = (s ++)
deparse _ _ (Lambda _) = id -- NINJA HIDDEN!
deparse _ _ (NumEntity e) = (en e ++)
    where en Pi = "pi"
          en Nabla = "nabla"
          en Infinite = "infinite"
deparse _ _ (CInteger i) = shows i
deparse _ _ (CFloat d) = shows d
deparse _ _ (Block i i1 i2) =
    ("block(" ++) . shows i . (',':) . shows i1 . (',' :) . shows i2 . (')' :)

deparse _ _ (App (Variable v) fl) =
    (v ++) . ('(' :) . argListToString fl . (')' :)

deparse _ _ (App f1 fl) =
    ('(' :) . deparse maxPrio False f1 . (")(" ++) . argListToString fl . (')' :)

deparse _ _ (Sum i i1 i2) =
    ("sum(" ++) . argListToString [i, i1, i2] . (')':)

deparse _ _ (Product i i1 i2) =
    ("product(" ++) . argListToString [i, i1, i2] . (')':)

deparse _ _ (Derivate i i1) =
    ("derivate(" ++) . argListToString [i, i1] . (')':)

deparse _ _ (Integrate i i1 i2 i3) =
    ("integrate(" ++) . argListToString [i, i1, i2, i3] . (')':)

deparse _ _ (UnOp OpFactorial f) = ('(':) . deparse maxPrio False f . (")!" ++)
deparse _ _ (UnOp op f) =
    ((++) $ unopString op ) . 
        ('(':) . deparse maxPrio False f . (')':)

deparse _ _ (Fraction f) =
    ('(':) . shows (numerator f)
           . ('/':)
           . shows (denominator f)
           . (')':)

 -- Special case... as OpEq is right associative...
 -- we must reverse shit for serialisation
deparse oldPrio right (BinOp OpEq [f1,f2]) =
    let (prio, txt) = (OpEq `obtainProp` Priority, binopString OpEq)
    in
    if prio > oldPrio || (not right && prio == oldPrio)
       then ('(':) 
                . deparse prio False f1 
                . (' ' :) . (txt ++) . (' ':) 
                . deparse prio True f2 . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio True f2

deparse oldPrio right (BinOp op [f1,f2]) =
    let (prio, txt) = (op `obtainProp` Priority, binopString op)
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then ('(':) . deparse prio False f1 
                . (' ' :) . (txt ++) . (' ':) 
                . deparse prio True f2 . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio True f2

deparse oldPrio right (BinOp op (f1:xs)) =
    let (prio, txt) = (op `obtainProp` Priority, binopString op)
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then ('(':) . deparse prio False f1 
                . (' ':) . (txt ++) . (' ':) 
                . deparse prio False (BinOp op xs) . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio False (BinOp op xs)

deparse _ _ (Matrix n m fl) =
    ("matrix("++) . shows n 
                  . (',':) 
                  . shows m 
                  . (',':) .  (argListToString $ concat fl) . (')':)

