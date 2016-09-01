import           Control.Monad.State
import           Data.Char
import qualified Data.Map            as M
import           Data.String.Utils
import           Scheme
import           System.IO
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Printf

-- Mini Schemeのプログラムリスト
miniScheme :: Parsec String u [SExp]
miniScheme = many (sexp <* spaces)

-- S式
sexp :: Parsec String u SExp
sexp = sint <|> ssym <|> sstr <|> squote <|> slist

-- 整数
sint :: Parsec String u SExp
sint = do
  spaces
  intStr <- many1 digit
  return . int . read $ intStr

-- シンボル
ssym :: Parsec String u SExp
ssym = do
  spaces
  symStr <- many1 (satisfy (\x -> isLetter x || x `elem` "+*?")) -- TODO 識別子でどの文字まで許されるか知る必要がある（+*?は利用可能）
  return . sym $ symStr

-- 文字列
sstr :: Parsec String u SExp
sstr = do
  spaces
  s <- between (char '"') (char '"') (many $ noneOf "\"") -- TODO エスケープは考慮していない
  return . str . replace "\\n" "\n" $ s

-- リスト
slist :: Parsec String u SExp
slist = do
  spaces
  char '('
  spaces
  ss <- sexp `sepBy` spaces
  spaces
  char ')'
  return $ foldr (:.) nil ss

-- quote (')
squote :: Parsec String u SExp
squote = do
  spaces
  char '\''
  x <- sexp
  return . quote $ x


test1 = do
  parseTest sint "10"
  parseTest ssym "x"
  parseTest sstr "\"hello, world\""
  parseTest squote "'10"
  parseTest slist "(1 2 3)"

  parseTest sexp "'(1 2 3)"
  parseTest sexp "()"
  parseTest sexp "(+ 2 3)"
  parseTest sexp "(null? ())"
  parseTest sexp "(define a (car '(1 2 3)))"
  parseTest sexp "(display \"hello, world\n\")"




main :: IO ()
main = do
  sourceCode <- readFile "test.scm"
  print sourceCode
  -- let sourceCode2 = "10\n(define x 1)\n(display x)\n(display \"\nhello, world\")"
  -- putStrLn sourceCode2
  -- sourceCode <- return "10\n"
  print sourceCode
  case parse miniScheme "" sourceCode of
    Right exps -> do
      let res = runStateT (forM exps eval) (M.empty, "")
      case res of
        Right (evaledExps, (env, stdout)) -> do
          printf "Evaled Exps: %s\n" (show evaledExps)
          printf "Env: %s\n" (show env)
          printf "Stdout: \n%s\n" stdout
        Left cause                       -> do
          putStrLn ("failed: " ++ cause)
    -- パースの失敗
    Left cause -> print cause
