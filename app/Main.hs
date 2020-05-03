module Main where

import Interpreter
import System.Environment
import System.IO (hPutStrLn, stderr, stdout)

main = do
    args <- getArgs
    input <- readFile (head args)
    let output = calc input
    let channel = case output of
            Left err -> stderr
            Right val -> stdout
    let text = case output of
            Left err -> show err
            Right val -> show val
    hPutStrLn channel text
