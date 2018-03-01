import Lexer
import Helper
import Bst
import Parser

main :: IO ()
main = do
    filecontent <- readFile "./Example.fyp"
    mapM_ putStrLn
      . astbuild
      . parse'
      . tokensbuild
      . concat $ [l | l <- lines filecontent, not (iscomment l)]
    -- mapM_ putStrLn . astbuild . parse' . rmemptyblock . splitbyterminator . rmemptytoken . tokenize . concat $ [x | x <- lines filecontent, not (iscomment x)]
    
