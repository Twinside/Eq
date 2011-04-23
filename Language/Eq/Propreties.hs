{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Eq.Propreties( Property( .. )
                          , TypeInfo( .. )
                          , obtainProp
                          ) where

import Data.Maybe

-- | Class to attach static propreties to a type
-- minimum definition : getProps
class (Eq propKey) => Property onType propKey propVal 
        | propKey -> propVal where
    -- | To retrieve all the propreties
    -- of the current item
    getProps :: onType -> [(propKey, propVal)] 

    -- | retrieve a propretie if it exists
    getProp :: onType -> propKey -> Maybe propVal
    getProp a what = lookup what $ getProps a

    -- | Tell if the element as the propreties
    -- passed as parameters
    hasProp :: onType -> propKey -> Bool
    hasProp a p = case getProp a p of
        Nothing -> False
        Just _ -> True

-- | Associate an unique meta information
-- to a type/value
class TypeInfo onType infoToken tokenType where
    propOf :: onType -> infoToken -> tokenType

obtainProp :: (Property a p c) => a -> p -> c
obtainProp a = fromJust . getProp a

