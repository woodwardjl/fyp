module Token where

import qualified Types as T
import Helper
import Operators

token :: T.Lexeme -> T.Token
token []                = T.Null
token xs
    | istype       xs   = T.TokenInt $ strtoint xs
    | isdatakeyword xs  = T.TokenData $ getvar xs
    | isoperator   xs   = T.TokenOperator $ op xs
    | isliteral    xs   = T.TokenInt $ strtoint xs
    | iswhitespace xs   = T.Null
    | otherwise         = case xs of
                          ";" -> T.TokenTerminator
                          _   -> T.Null
