module Envlambda where

import Data.Bifunctor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Control.Monad

type Name = [Char]

data Expression =
    MakeLambda Name       Expression
  | Call       Expression Expression
  | EnvRef     Name

instance Show Expression where
  show (MakeLambda argname body) =
      "(lambda (" ++ argname ++ ") " ++ show body ++ ")"
  show (Call function argument) =
      "(" ++ show function ++ " " ++ show argument ++ ")"
  show (EnvRef name) = name

data Lambda = Lambda { argumentname :: Name, contents :: Expression, parentEnv :: Environment }

instance Show Lambda where
  show (Lambda argumentname contents parentEnv) =
      "(lambda (" ++ argumentname ++ ") " ++ show contents ++ ")"

data Environment =
    Root
  | Environment Name Lambda Environment

envLookup :: Name -> Environment -> Lambda
envLookup n (Root) = error $ "Yikes, I couldn't find the name " ++ n
envLookup n (Environment key value parent) = if n == key then value else (envLookup n parent)

evalExp :: Environment -> Expression -> Lambda

evalExp env (MakeLambda argname body) = Lambda argname body env
evalExp env (EnvRef name) = envLookup name env
evalExp env (Call function argument) =
  let arg = evalExp env argument
      fn  = evalExp env function
      ne  = Environment (argumentname fn) arg (parentEnv fn)
  in  evalExp ne (contents fn)

eval :: Expression -> Lambda
eval exp = evalExp Root exp

define :: Name -> Expression -> Expression -> Expression
define name value next = (Call (MakeLambda name next) value)

cl :: [Name] -> Expression -> Expression
cl [n] body = MakeLambda n body
cl (n:rest) body = MakeLambda n (cl rest body)

cc :: Expression -> [Expression] -> Expression
cc e [arg] = Call e arg
cc e (arg:rest) = cc (Call e  arg) rest

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

data Stmt = Define Name Expression
          | Bare Expression
          deriving Show

nameP :: Parser Name
nameP = lexeme $ some letterChar

symbol :: String -> Parser String
symbol = L.symbol space

lambdaP :: Parser Expression
lambdaP = do _ <- symbol "λ"
             args' <- some nameP
             _ <- symbol "."
             body <- exprP
             return $ cl args' body

exprP' :: Parser Expression
exprP' = symbol "(" *> exprP <* symbol ")"
     <|> lambdaP
     <|> (EnvRef <$> nameP)

exprP :: Parser Expression
exprP = lexeme $ do (expr':exprs) <- some exprP'
                    return $ if null exprs then expr' else cc expr' exprs

stmtP :: Parser Stmt
stmtP = try (Define <$> nameP <* symbol "=" <*> exprP)
    <|> Bare <$> exprP

pars :: String -> Expression
pars str = case parse exprP "" str of
         Right expr -> expr
         Left err -> error (errorBundlePretty err)

pars' :: String -> Stmt
pars' str = case parse stmtP "" str of
              Right expr -> expr
              Left err -> error (errorBundlePretty err)

type Env = Expression -> Expression

eval' :: Env -> Stmt -> (Env, Lambda)
eval' env (Define n e) = (env . define n e, eval $ env e)
eval' env (Bare e) = (env, eval $ env e)

evalStmt :: Stmt -> Env -> Lambda
evalStmt (Define _ e) = \env -> eval $ env e
evalStmt (Bare e) =  \env -> eval $ env e

repl :: IO ()
repl = loop id
  where
    loop env = do
      putStr ">>> "
      hFlush stdout
      inp <- getLine
      when (inp /= "exit") $
        let (env', lam) = eval' env $ pars' inp
        in print lam >> loop env'

(&&&) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(&&&) = bimap

-- eval'' :: (Env, Stmt) -> (Env, Lambda)
-- eval'' a@(env, Define n e) = ((. define n e) &&& evalStmt env) a
-- eval'' a@(env, Bare _) = (id &&& evalStmt env) a

-- pars :: Parser Expression
-- pars = do exprs <- sepBy (Define <$> nameP <* symbol "=" <*> exprP) (symbol "|")
