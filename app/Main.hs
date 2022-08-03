module Main where

import System.Environment
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Lambdacalc

data Command = REPL ReplOpts
             | Run RunOpts

-- Written like so to allow for easy addition of features

newtype ReplOpts = ReplOpts {
  optfiles :: [FilePath]
}

newtype RunOpts = RunOpts {
  optfile :: FilePath
  -- optOut :: Maybe FilePath
}

newtype Options = Options {
  optComm :: Command
}

opts :: Options.Applicative.Parser Options
opts = Options <$> ( subparser (command "repl" (info (REPL <$> (reploptions <**> helper)) (progDesc "starts a lambdacalc repl")))
               <|>   Run <$> runoptions
                   )
  where
    reploptions = ReplOpts <$>
      many (argument str (metavar "FILES..." <> help "Files to load into the repl"))

    runoptions = RunOpts <$>
      argument str (metavar "FILE" <> help "File to run") -- <*>
      -- optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "File to output the displays into"))

main  :: IO ()
main  = do opts'  <- execParser optsParser
           case optComm opts' of
             REPL ops -> repl (optfiles ops)
             Run ops -> do env <- nullEnv
                           void (runIOExpr (eval env (Load [optfile ops])))
  where
    optsParser :: ParserInfo Options
    optsParser = info (opts <**> helper) (progDesc " ")

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             ["repl"] -> repl
--             [fn] -> do env <- nullEnv
--                        runIOExpr (eval env (Load [fn])) >>= putStrLn
--             (arg:_) -> putStrLn ("Unsupported command: " ++ arg) >> exitWith (ExitFailure 1)
--             [] -> putStrLn "Use help to find usage" >> exitWith (ExitFailure 1)

