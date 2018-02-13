module Errors where

import System.Exit

symbolerror :: String -> String -> IO ()
symbolerror x xs = do
    putStrLn ("error: " ++ xs ++ ": (" ++ x ++ ")")
    exitFailure
