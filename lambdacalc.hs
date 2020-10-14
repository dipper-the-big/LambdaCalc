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

data Stmt = Define Expr Expr
          | Display Expr

data Err = LookupErr String
         | ParseErr String

instance Show Err where
  show (LookupErr name) = "Could not fetch unbound variable" ++ name
  show (ParseErr err) = "parse error:\n" ++ err

type ErrH = Either Err

type IOErrH = ExceptT Err IO

type Env = IORef (Map.Map String (IORef Expr))

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
stmtP = try (Define <$> identifierP <* symbol "=" <*> exprP)
    <|> symbol "display:" *> (Display <$> exprP)

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

 -- <|> symbol 'λ' *> space *> liftM2 (foldr Lambda) (symbol '.' *> space *> exprP) (some (identifierP <* space))


tagify :: Int -> Expr -> Expr
tagify n (Lambda i body) = Lambda (Tag n) $ tagify (n + 1) (substitute body i (Tag n))
tagify _ (App exp1 exp2) = App (tagify 0 exp1) (tagify 0 exp2)
tagify _ i = i

par :: String -> Expr
par a = tagify 0 $ case parse exprP "" a of
                     Right expr -> expr
                     Left err -> error $ errorBundlePretty err

pars :: String -> Stmt
pars a = case parse stmtP "" a of
           Right expr -> expr
           Left err -> error $ errorBundlePretty err

substitute :: Expr -> Expr -> Expr -> Expr
substitute l@(Lambda x expr) old new = if x == old then l else Lambda x $ substitute expr old new
substitute (App exp1 exp2) old new = App (substitute exp1 old new) (substitute exp2 old new)
substitute i old new = if i == old then new else i
-- substitute i@(Identifier name) old new = if name == old then new else i

eval :: Expr -> Expr
eval (App (Lambda x body) expr) = eval $ substitute body x expr
eval (App exp1 exp2) = eval $ App (eval exp1) (eval exp2)
eval (Lambda x expr) = tagify 0 $ Lambda x (eval expr)
-- eval (Identifier _) = error "sorry no bound variables yet"
eval expr = expr

teval :: String -> Expr
teval expr = eval $ par expr
