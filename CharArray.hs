{-# LANGUAGE FlexibleContexts #-}
module CharArray where

import Data.Array.IArray

lineOfArray :: (Enum i, Ix i, IArray a Char)
            => a (i,i) Char -> i -> String
lineOfArray a i = [ a ! (x, i) | x <- [xMin .. xMax]]
        where ((xMin,_),(xMax,_)) = bounds a

linesOfArray :: (Enum i, Ix i, IArray a Char)
             => a (i,i) Char -> [String]
linesOfArray a = map (lineOfArray a) [yMin .. yMax]
    where ((_,yMin),(_, yMax)) = bounds a

charArrayToString :: (Enum i, Ix i, IArray a Char)
                  => a (i,i) Char -> String
charArrayToString = concat . reverse 
                  . map (++ "\n") . linesOfArray

