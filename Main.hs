import Lexer
import Helper
import Errors
import Parser

main :: IO ()
main = do
    filecontent <- readFile "./Example.fyp"
    let tokens  = tokenize $ concat [l | l <- lines filecontent, not (iscomment l)] 
    let invalid = isinvalid tokens
    if fst invalid
        then symbolerror (snd invalid) "undefined symbol"
        else print $ eval' $ eval tokens

