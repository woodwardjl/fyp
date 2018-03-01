import Lexer
import Helper
import Parser
import AST

main :: IO ()
main = do
    filecontent <- readFile "./Example.fyp"
    mapM_ putStrLn
      . astbuild
      . parse'
      . tokensbuild
      . concat $ [l | l <- lines filecontent, not (iscomment l)]
