module Token where

import qualified Types as T
import Helper
import TypeHelper

token :: T.Lexeme -> T.Token
token []                       = T.Null
token xs
    | isconditionalkeyword xs  = T.TokenConditional $ conditional xs
    | iscomparsionkeyword xs   = T.TokenComparison $ comparison xs
    | xs == "def"              = T.TokenDefiner xs
    | isdatakeyword xs         = T.TokenData xs (getvar xs)
    | isoperator   xs          = T.TokenOperator $ op xs
    | isliteral    xs          = T.TokenInt $ strtoint xs
    | iswhitespace xs          = T.Null
    | otherwise                = case xs of
                                   "("  -> T.TokenBraceL
                                   ")"  -> T.TokenBraceR
                                   _    -> error $ "invalid symbol/value: " ++ xs
