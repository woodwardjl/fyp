module Token where

import qualified Types as T
import Helper

token :: T.Lexeme -> T.Token
token []              = (T.Null, "")
token xs
    | istype       xs = (T.Type, xs)
    | iskeyword    xs = (T.Keyword, xs)
    | isoperator   xs = (T.Operator, xs)
    | isliteral    xs = (T.Literal, xs)
    | iswhitespace xs = (T.Null, "")
    | otherwise       = case xs of
                          ";"    -> (T.Terminator, xs)
                          "{"    -> (T.Block, xs)
                          "}"    -> (T.Block, xs)
                          "plus" -> (T.Operator, xs)
                          "neg"  -> (T.Operator, xs)
                          ":"    -> (T.Definer, xs)
                          "."    -> (T.Accessor, xs)
                          _      -> (T.Null, xs)
