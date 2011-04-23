module Language.Eq.FormulaIterator where

import Control.Applicative
import Language.Eq.Types

depthFirstFormula :: (Applicative m, Monad m) 
                  => (Formula a -> m (Formula b)) -> Formula a -> m (Formula b)

depthFormulaTraversal :: (Applicative m, Monad m)
                      => (Formula a -> m ())
                      -> (Formula a -> m (Formula b))
                      -> Formula a -> m (Formula b)

depthFormulaPrimTraversal :: (Applicative m, Monad m)
                          => (FormulaPrim -> m FormulaPrim)
                          -> FormulaPrim
                          -> m FormulaPrim

topDownTraversal :: (FormulaPrim -> Maybe FormulaPrim)
                 -> FormulaPrim
                 -> FormulaPrim

depthPrimTraversal :: (Applicative m, Monad m) 
                   => (FormulaPrim -> m ()) 
                   -> (FormulaPrim -> m FormulaPrim)
                   -> FormulaPrim
                   -> m FormulaPrim
