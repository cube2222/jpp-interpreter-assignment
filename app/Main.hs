module Main where

import Interpreter
import System.Environment
import System.IO (hPutStrLn, stderr, stdout)

calcStr str = case calc str of
    Left err -> show err
    Right val -> show val

main = do
    args <- getArgs
    text <- readFile (head args)
    output <- calc text
    let channel = case output of
        Left err -> stderr
        Right val -> stdout
    let text = case output of
        Left err -> show err
        Right val -> show val
    hPutStrLn channel output
