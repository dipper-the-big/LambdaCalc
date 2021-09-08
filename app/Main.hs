module Main where

import System.Environment
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Lambdacalc

data Command = REPL ReplOpts
             | Run RunOpts

data ReplOpts = ReplOpts {
  optfiles :: [FilePath]
}

data RunOpts = RunOpts {
  optfile :: FilePath,
  optOut :: FilePath
}

data Options = Options {
  optComm :: Command
  -- optOut ::
  -- optfile :: Maybe FilePath
}

opts :: Options.Applicative.Parser Options
opts = Options <$>
       subparser ( command "repl" (info (REPL <$> (reploptions <**> helper)) (progDesc "starts a lambdacalc repl"))
                <> command "run" (info (Run <$> (runoptions <**> helper)) (progDesc "runs a lambdacalc file"))
                 )
  where
    reploptions = ReplOpts <$>
      many (argument str (metavar "FILES..." <> help "Files to load into the repl"))

    runoptions = RunOpts <$>
      argument str (metavar "FILE" <> help "File to run") <*>
      argument str (metavar "OUTPUT" <> help "File to output the displays into")

main  :: IO ()
main  = do opts'  <- execParser optsParser
           case optComm opts' of
             REPL ops -> repl (optfiles ops)
             Run ops -> do env <- nullEnv
                           Control.Monad.void (runIOExpr (eval env (Load [optfile ops])))
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

