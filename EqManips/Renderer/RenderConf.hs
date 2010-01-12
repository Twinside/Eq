module EqManips.Renderer.RenderConf( confLoad
                                   , Conf( .. )
                                   , defaultRenderConf
                                   ) where

import Data.Char( isSpace )

data Conf = Conf
    { mulAsDot :: Bool
    , packNumVarMul :: Bool
    , noBigOpOverSize :: Bool
    }

defaultRenderConf :: Conf
defaultRenderConf = Conf
    { mulAsDot = True
    , packNumVarMul = False
    , noBigOpOverSize = False
    }

keyParser :: [(String, Conf -> String -> Conf)]
keyParser =
    [ ("mulasdot"       , \c v -> c{ mulAsDot = permissiveBool v } )
    , ("packnumvarmul"  , \c v -> c{ packNumVarMul = permissiveBool v} )
    , ("nobigopoversize", \c v -> c{ noBigOpOverSize = permissiveBool v} )
    ]

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

permissiveBool :: String -> Bool
permissiveBool "1" = True
permissiveBool "yes" = True
permissiveBool "true" = True
permissiveBool "True" = True
permissiveBool _ = False

confRead :: String -> Conf -> Conf
confRead ('#':_) c = c
confRead s c = case lookup (trim key) keyParser of
        Just parser -> parser c $ trim value
        Nothing -> c
    where (key, value) = break ('=' ==) s

confLoad :: [String] -> Conf
confLoad = foldr confRead defaultRenderConf

