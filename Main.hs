import Lexer
import Helper
import Errors

main :: IO ()
main = do
    filecontent <- readFile "./Example.fyp"
    -- print . parse' . rmemptyblock . splitbyterminator . rmemptytoken . tokenize $ concat [l | l <- lines filecontent, not (iscomment l)]
    print $ rmemptyblock $ splitbyterminator $ rmemptytoken $ tokenize $ concat [l | l <- lines filecontent, not (iscomment l)]
    
