module EqManips.Algorithm.Polynome ( ) where

{-import EqManips.Types-}
{-import EqManips.Algorithm.Utils-}

-- |
-- We assume that the formula as been previously sorted
{-resign (BinOp OpMul (CInteger n: xs)) = BinOp OpMul $ CInteger (-n) : xs-}
{-resign (BinOp OpMul (CFloat n: xs)) = BinOp OpMul $ CFloat (-n) : xs-}
{-resign (BinOp OpMul (a:xs)) | isFormulaConstant a = BinOp OpMul $ CInteger (-1):a:xs-}
{-resign a = BinOp OpMul [CInteger (-1), a]-}

-- | Given a formula in LIST form
{-listFlatter :: FormulaPrim -> FormulaPrim-}
{-listFlatter (BinOp OpAdd lst) = BinOp OpAdd $ foldr flatter [] lst-}
    {-where flatter (BinOp OpSub (x:xs)) acc = x : map resign xs ++ acc-}
          {-flatter x acc = x:acc-}
{-listFlatter a = a-}

