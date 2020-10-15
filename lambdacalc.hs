import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.IORef
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import System.IO

type Parser = Parsec Void String

data Expr = App Expr Expr
          | Lambda Expr Expr
          | Identifier String
          | Tag Int
          deriving (Eq,Show)

data Stmt = Define String Expr
          | Display Expr
          | Equal Expr Expr
          | Bare Expr

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

exprP' :: Parser Expr
exprP' = symbol "(" *> exprP <* symbol ")"
     <|> lambdaP
     <|> identifierP

exprP :: Parser Expr
exprP = lexeme $ foldl1 App <$> some exprP'

stmtP :: Parser Stmt
stmtP = lexeme
         $  try (Define <$> identifierP' <* symbol "=" <*> (tagify 0 <$> exprP))
        <|> symbol "display:" *> (Display <$> (tagify 0 <$> exprP))
        <|> try (Equal <$> (tagify 0 <$> exprP) <* symbol "≡" <*> (tagify 0 <$> exprP))
        <|> Bare <$> (tagify 0 <$> exprP)

identifierP' :: Parser String
identifierP' = lexeme $ some letterChar

identifierP :: Parser Expr
identifierP  = Identifier <$> identifierP'

lambdaP :: Parser Expr
lambdaP = try (Lambda <$ symbol "λ" <*> identifierP <* symbol "." <*> exprP)
      <|> do _ <- symbol "λ"
             args <- some identifierP
             _ <- symbol "."
             body <- exprP
             return $ foldr Lambda body args

par' :: String -> Expr
par' a = tagify 0 $ case parse exprP "" a of
                      Right expr -> expr
                      Left err -> error $ errorBundlePretty err

par :: String -> IOErrH Expr
par a = fmap (tagify 0) $ case parse exprP "" a of
                            Right expr -> return expr
                            Left err -> throwError $ ParseErr (errorBundlePretty err)

readExpr :: String -> IOErrH Stmt
readExpr a = case parse stmtP "" a of
               Right expr -> return expr
               Left err -> throwError $ ParseErr (errorBundlePretty err)

-- Evaluation

tagify :: Int -> Expr -> Expr
tagify n (Lambda i body) = Lambda (Tag n) $ tagify (n + 1) (substitute body i (Tag n))
tagify n (App exp1 exp2) = App (tagify 0 exp1) (tagify 0 exp2)
tagify _ i = i

substitute :: Expr -> Expr -> Expr -> Expr
substitute l@(Lambda x expr) old new = if x == old then l else Lambda x $ substitute expr old new
substitute (App exp1 exp2) old new = App (substitute exp1 old new) (substitute exp2 old new)
substitute i old new = if i == old then new else i

evalExpr :: Env -> Expr -> IOErrH Expr
evalExpr env (App (Lambda x body) expr) = evalExpr env (substitute body x expr)
evalExpr env e@(App exp1 exp2) = do e1 <- evalExpr env exp1
                                    e2 <- evalExpr env exp2
                                    if e1 == exp1 && e2 == exp2
                                      then return e
                                      else evalExpr env (App e1 e2)
-- evalExpr _ (Lambda x expr) = return $ Lambda x expr -- <$> evalExpr env expr
evalExpr env (Lambda x expr) =  Lambda x <$> evalExpr env expr
evalExpr env (Identifier x) = getvar env x
evalExpr _ expr = return expr

eval :: Env -> Stmt -> IOErrH String
eval env (Define name expr) = show . tagify 0 <$> (evalExpr env expr >>= definevar env name)
eval env (Display expr) = show . tagify 0 <$> evalExpr env expr
eval env (Equal exp1 exp2) = do e1 <- tagify 0 <$> (evalExpr env =<< unid env exp1)
                                e2 <- tagify 0 <$> (evalExpr env =<< unid env exp2)
                                return $ show (e1 == e2)
  where
    unid env' i@(Identifier _) = evalExpr env' i
    unid _ i = return i
eval env (Bare expr) = show . tagify 0 <$> evalExpr env expr

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
nullEnv = newIORef $ Map.fromList [ ("I",Lambda (Tag 0) (Tag 0))
                                  , ("K",Lambda (Tag 0) (Lambda (Tag 1) (Tag 0)))
                                  , ("S",Lambda (Tag 0) (Lambda (Tag 1) (Lambda (Tag 2) (App (App (Tag 0) (Tag 2)) (App (Tag 1) (Tag 2))))))
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
     case Map.lookup var env' of
       Nothing -> error "Wow, something really went wrong"
       Just _ -> liftIO $ writeIORef env $ Map.adjust (const val) var env'
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
