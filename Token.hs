module Token where

import Types
import Helper

token :: Lexeme -> MToken
token []                       = (MEmpty, " ")
token xs
    | istype       xs          = (MType, xs)
    | isdatakeyword xs         = (MDataKeyword, xs)
    | isconditionalkeyword xs  = (MConditionalKeyword, xs)
    | iskeyword    xs          = (MKeyword, xs)
    | isoperator   xs          = (MOperator, xs)
    | isliteral    xs          = (MLiteral, xs)
    | iswhitespace xs          = (MEmpty, "")
    | otherwise                = case xs of
                                    ";" -> (MTerminator, xs)
                                    "{" -> (MBlockS, xs)
                                    "}" -> (MBlockF, xs)
                                    "-" -> (MOperator, xs)
                                    "+" -> (MOperator, xs)
                                    ":" -> (MAssignment, xs)
                                    "." -> (MAccessor, xs)
                                    _   -> (MUndefined, xs)