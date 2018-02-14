import Lexer
import Helper
import Errors
import Parser
import qualified Types as T

main :: IO ()
main = do
    filecontent <- readFile "./Example.fyp"
    let tokens  = tokenize $ concat [l | l <- lines filecontent, not (iscomment l)] 
    let invalid = isinvalid tokens
    -- ^assert brace tuple values equal: (\(x, y) -> x == y)
    if fst invalid
        then symbolerror (snd invalid) "undefined symbol"
        else mapM_ print $ 
             rmemptyblock        $
             splitbyblock tokens $ bracecnt tokens
             
              
