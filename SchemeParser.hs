import           Data.Char
import           Scheme
import           Text.Parsec
import           Text.Parsec.Char


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
  return . str $ s

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



main :: IO ()
main = do
  test1
