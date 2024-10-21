module Main where

import Control.Monad (when)
import Eval
import Parser
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

quit :: String -> IO ()
quit c =
    when (c == "quit" || c == "q") $ do
        exitSuccess

repl :: IO ()
repl = do
    putStr "rc> "
    hFlush stdout
    c <- getLine
    quit c
    let res = parseExpr c
    case res of
        Left err -> putStrLn err >> repl
        Right val -> print' (eval val) >> repl
  where
    print' :: [Molecule] -> IO ()
    print' = print

main :: IO ()
main = putStrLn "Welcome to biolang!" >> repl
