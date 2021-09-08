module Lambdacalc where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import System.IO

type Parser = Parsec Void String

data Expr = App Expr Expr
          | Lambda Expr Expr
          | Identifier String
          | Tag (Int,Int)
          deriving Eq

instance Show Expr where
  show (App e l@(Lambda _ _)) = show e ++ " (" ++ show l ++ ") "
  show (App e a@(App _ _)) = show e ++ " (" ++ show a ++ ")"
  show (App a@(App _ _) e3) = show a ++ " " ++ show e3
  show (App e1 e2) = show e1 ++ " " ++ show e2
  show (Lambda x body) = "λ" ++ show x ++ "." ++ show body
  show (Tag l) = [taglabel l]
  -- show (Tag l) = show l
  show (Identifier x) = x

taglabel :: (Int, Int) -> Char
taglabel (a,b) = ['a'..'z'] !! fromMaybe 0 (elemIndex (a,b) ls)
  where
    n_part n = zip [0..(n - 1)] (repeat n) ++ zip (repeat n) [0..(n - 1)] ++ [(n,n)]
    ls = (0,0) : concatMap n_part [1..]

data Stmt = Define String Expr
          | Load [String]
          | Display Expr
          | Expand Expr
          | Equal Expr Expr
          | Bare Expr
          deriving Show

data Err = LookupErr String
         | ParseErr String
         deriving Eq

instance Show Err where
  show (LookupErr name) = "Could not fetch unbound variable: " ++ name
  show (ParseErr err) = "parse error:\n" ++ err

type ErrH = Either Err

type IOErrH = ExceptT Err IO

type Env = IORef (Map.Map String Expr)

-- Parsing

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

identifierP' :: Parser String
identifierP' = lexeme $ some letterChar

identifierP :: Parser Expr
identifierP  = Identifier <$> identifierP'

lambdaP :: Parser Expr
lambdaP = try (Lambda <$ (symbol "λ" <|> symbol "\\") <*> identifierP <* symbol "." <*> exprP)
      <|> do _ <- symbol "λ" <|> symbol "\\"
             args <- some identifierP
             _ <- symbol "."
             body <- exprP
             return $ foldr Lambda body args

exprP' :: Parser Expr
exprP' = symbol "(" *> exprP <* symbol ")"
     <|> lambdaP
     <|> identifierP

exprP :: Parser Expr
exprP = lexeme $ foldl1 App <$> some exprP'

stmtP :: Parser Stmt
stmtP = lexeme
         $  symbol "expand:" *> (Expand <$> exprP)
        <|> symbol "display:" *> (Display <$> exprP)
        <|> symbol "load" *> (Load <$> sepBy (some $ anySingleBut ' ') space)
        <|> (try (Equal <$> exprP <* (symbol "≡" <|> symbol "===")) <*> exprP)
        <|> (try (Define <$> identifierP' <* symbol "=") <*> exprP)
        <|> Bare <$> exprP

readH :: Parser a -> String -> IOErrH a
readH parser str = case parse parser "" str of
                     Right expr -> return expr
                     Left err -> throwError $ ParseErr (errorBundlePretty err)

readExpr :: String -> IOErrH Stmt
readExpr = readH (space *> stmtP)

readExprList :: String -> IOErrH [Stmt]
readExprList = readH $ lexeme (sepBy stmtP (symbol ","))

load :: FilePath -> IOErrH [Stmt]
load filename = liftIO (readFile filename) >>= readExprList

-- Evaluation

-- unId :: Env -> Expr -> IOErrH Expr
-- unId env (Identifier x) = getvar env x >>= unId env
-- unId env (App a b) = liftM2 App (unId env a) (unId env b)
-- unId env (Lambda x expr) =  Lambda x <$> unId env expr
-- unId _ i = return i

-- tagify :: Int -> Int -> Expr -> Expr
-- tagify e n (Lambda i body) = Lambda (Tag (e,n)) $ tagify e (n + 1) (substitute body i (Tag (e,n))) -- Fix this: the substitute substitutes some stuff that it shouldn't
--                                                                                                    -- E.G a tag 00 could replace an unid'd expr's tag 00
-- tagify e _ (App exp1 exp2) = App (tagify (e + 2) 0 exp1) (tagify (e + 1) 0 exp2)
-- tagify _ _ i = i

tagify :: Int -> Int -> Env -> Expr -> IOErrH Expr
tagify e n env (Lambda i body) = Lambda (Tag (e,n)) <$> tagify e (n + 1) env (substitute body i (Tag (e,n)))
tagify e _ env (App exp1 exp2) = liftM2 App (tagify (e + 2) 0 env exp1) (tagify (e + 1) 0 env exp2)
tagify _ _ _ t@(Tag _) = return t
tagify e n env (Identifier name) = getvar env name >>= tagify (e + 1) n env

-- pp :: Env -> Expr -> IOErrH Expr
-- pp env expr = tagify 0 0 <$> unId env (tagify 0 0 expr)

substitute :: Expr -> Expr -> Expr -> Expr
substitute l@(Lambda x expr) old new = if x == old then l else Lambda x $ substitute expr old new
substitute (App exp1 exp2) old new = App (substitute exp1 old new) (substitute exp2 old new)
substitute i old new = if i == old then new else i

-- helper for evaluation trace
tevalExpr :: Env -> Expr -> IO Expr
tevalExpr env e@(App (Lambda x body) expr) = liftIO $ print e >> tevalExpr env (substitute body x expr)
tevalExpr env e@(App exp1 exp2) = liftIO $ print e >> do e1 <- tevalExpr env exp1
                                                         e2 <- tevalExpr env exp2
                                                         if e1 == exp1 && e2 == exp2
                                                           then return e
                                                           else tevalExpr env (App e1 e2)
tevalExpr env e@(Lambda x expr) = liftIO $ print e >> Lambda x <$> tevalExpr env expr
tevalExpr _ expr = liftIO $ print expr >> return expr

evalExpr :: Env -> Expr -> IOErrH Expr
evalExpr env (App (Lambda x body) expr) = evalExpr env (substitute body x expr)
evalExpr env e@(App exp1 exp2) = do e1 <- evalExpr env exp1
                                    e2 <- evalExpr env exp2
                                    if e1 == exp1 && e2 == exp2
                                      then return e
                                      else evalExpr env (App e1 e2)
evalExpr env (Lambda x e@(App expr y)) = if x == y then return expr else Lambda x <$> evalExpr env e
evalExpr env (Lambda x expr) =  Lambda x <$> evalExpr env expr
evalExpr env (Identifier x) = getvar env x
evalExpr _ expr = return expr

eval :: Env -> Stmt -> IOErrH String
eval env (Define name expr) = show <$> (tagify 0 0 env expr >>= evalExpr env >>= definevar env name >>= tagify 0 0 env)
eval env (Load fs) = do  mapM_ (load >=> mapM_ (eval env)) fs
                         -- forM_ fs $ \fn -> do
                         --   load fn >>= mapM_ (eval env)
                         return "success"
eval env (Display expr) = (tagify 0 0 env expr >>= evalExpr env >>= tagify 0 0 env) >>= (liftIO . print) >> return ""
eval env (Expand expr) = show <$> tagify 0 0 env expr
eval env (Equal exp1 exp2) = do e1 <- tagify 0 0 env =<< evalExpr env =<< tagify 0 0 env exp1
                                e2 <- tagify 0 0 env =<< evalExpr env =<< tagify 0 0 env exp2
                                return $ show (e1 == e2)
eval env (Bare expr) = show <$> (tagify 0 0 env expr >>= evalExpr env >>= tagify 0 0 env)

extract :: ErrH String -> String
extract = extract'' . extract'
  where
    extract' action = catchError action (return . show)
    extract'' (Right a) = a
    extract'' (Left _) = "Oops This wasn't sposed to happen"

runIOExpr :: IOErrH String -> IO String
runIOExpr action = extract <$> runExceptT action

-- Environment

nullEnv :: IO Env
nullEnv = newIORef $ Map.fromList [ ("I",Lambda (Tag (0,0)) (Tag (0,0)))
                                  , ("K",Lambda (Tag (0,0)) (Lambda (Tag (0,1)) (Tag (0,0))))
                                  , ("S",Lambda (Tag (0,0)) (Lambda (Tag (0,1)) (Lambda (Tag (0,2)) (App (App (Tag (0,0)) (Tag (0,2))) (App (Tag (0,1)) (Tag (0,2)))))))
                                  ]

getvar :: Env -> String -> IOErrH Expr
getvar env var =
  do env' <- liftIO $ readIORef env
     case Map.lookup var env' of
       Nothing -> throwError $ LookupErr var
       Just varref -> return varref

isBound :: Env -> String -> IO Bool
isBound env var = do env' <- readIORef env
                     case Map.lookup var env' of
                       Nothing -> return False
                       Just _ -> return True

setvar :: Env -> String -> Expr -> IOErrH Expr
setvar env var val =
  do env' <- liftIO $ readIORef env
     liftIO $ writeIORef env $ Map.insert var val env'
     return val

definevar :: Env -> String -> Expr -> IOErrH Expr
definevar env var val =
  do alreadyDefined <- liftIO $ isBound env var
     if alreadyDefined
       then setvar env var val >> return val
       else liftIO $ do env' <- readIORef env
                        writeIORef env $ Map.insert var val env'
                        return val

repl :: IO ()
repl = nullEnv >>= loop
  where
    loop env = do
      putStr ">>> "
      hFlush stdout
      inp <- getLine
      when (inp /= "exit") $
        runIOExpr (readExpr inp >>= eval env) >>= putStrLn >> loop env

-- Extras (For Debugging)
--
-- par' :: String -> Stmt
-- par' a = case parse stmtP "" a of
--                         Right expr -> expr
--                         Left err -> error $ errorBundlePretty err

-- par :: String -> IOErrH Expr
-- par a = fmap (tagify 0) $ case parse exprP "" a of
--                             Right expr -> return expr
--                             Left err -> throwError $ ParseErr (errorBundlePretty err)
--
-- eval' :: Expr -> Expr
-- eval' (App (Lambda x body) expr) = eval' $ substitute body x expr
-- eval' (App exp1 exp2) = eval' $ App (eval' exp1) (eval' exp2)
-- eval' (Lambda x expr) = Lambda x (eval' expr)
-- eval' expr = expr

-- teval' :: String -> Expr
-- teval' expr = tagify 0 $ eval' $ par' expr


-- teval :: String -> IO ()
-- teval expr = do env <- nullEnv
--                 res <- runIOExpr $ fmap (show . tagify 0) $ par expr >>= eval env
--                 putStrLn res

            -- fn -> do env <- nullEnv
            --          runIOExpr (eval env (Load fn)) >>= putStrLn >> loop env
