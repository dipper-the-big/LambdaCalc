module Main where

import System.Environment
import System.Exit
import Lambdacalc

main :: IO ()
main = do args <- getArgs
          case args of
            ["repl"] -> repl
            [fn] -> do env <- nullEnv
                       runIOExpr (eval env (Load [fn])) >>= putStrLn
            (arg:_) -> putStrLn ("Unsupported command: " ++ arg) >> exitWith (ExitFailure 1)
            [] -> putStrLn "Use help to find usage" >> exitWith (ExitFailure 1)
