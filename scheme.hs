-- コンパクトなSchemeのインタプリタを作る試み
-- ここを参考にしているhttp://www.geocities.jp/m_hiroi/func/haskell34.html

module Scheme(
  Number(..),
  Atom(..),
  SExp(..),
  eval,
  int,
  real,
  str,
  sym,
  t,
  f,
  nil,
  quote
) where

import           Control.Monad.State
import qualified Data.Map            as M
import           Text.Printf
import           Unsafe.Coerce

data Number = INT Integer | REAL Double deriving Eq
data Atom   = NUM Number | SYM String | STR String | Lam SExp SExp | T | F | UNDEF | NIL deriving (Eq) -- Lam（ラムダ）は最初が引数で残りが実装
data SExp   = Atm Atom | SExp :. SExp deriving (Eq)
infixr 0 :.

instance Num Number where
  INT  a + INT  b = INT  (a + b)
  INT  a + REAL b = REAL (fromIntegral a + b)
  REAL a + INT b  = REAL (a + fromIntegral b)
  REAL a + REAL b = REAL (a + b)

  INT  a * INT  b = INT  (a * b)
  INT  a * REAL b = REAL (fromIntegral a * b)
  REAL a * INT b  = REAL (a * fromIntegral b)
  REAL a * REAL b = REAL (a * b)

  negate (INT a)  = INT (-a)
  negate (REAL a) = REAL (-a)

  abs (INT a)     = INT (abs a)
  abs (REAL a)    = REAL (abs a)

  signum (INT a)  = INT (signum a)
  signum (REAL a) = REAL (signum a)

  fromInteger     = INT


instance Show Number where
  show (INT n)  = show n
  show (REAL n) = show n

-- アトムのShow
instance Show Atom where
  show(NUM n)  = show n
  show(SYM s)  = s
  show(STR s)  = show s
  show (Lam ps content) = "(lambda " ++ show ps ++ " " ++ show content ++ ")"
  show T       = "t"
  show F       = "f"
  show NIL     = "()"
  show UNDEF   = "<undef>"

-- S式のShow
instance Show SExp where
  show (Atm a)  = show a
  show as       = '(': showLis as ++")"
    where
      showLis :: SExp -> String
      showLis (car :. cdr) =
        show car ++ case cdr of
                     Atm NIL   -> ""
                     Atm a     -> "." ++ show a
                     _            -> " " ++ showLis cdr

-- シンボルの作成がし易いように
sym :: String -> SExp
sym = Atm . SYM

-- 整数が作りやすいように
int :: Integer -> SExp
int = Atm . NUM . INT

real :: Double -> SExp
real = Atm . NUM . REAL

nil :: SExp
nil = Atm NIL

t :: SExp
t = Atm T

f :: SExp
f = Atm F

str :: String -> SExp
str = Atm . STR


-- 変数名と値の関係
type Env = M.Map String [SExp]

-- Schemにおける出力先
type Stdout = String


eval :: SExp -> StateT (Env, Stdout) (Either String) SExp

eval(Atm (SYM "t")) = return $ Atm T
eval(Atm (SYM "f")) = return $ Atm F
eval(Atm (SYM "()")) = return $ Atm NIL


-- シンボルの時は変数の取得
eval(Atm (SYM xname)) = StateT (\(env, stdout) ->
    case M.lookup xname env of
      Just ex -> Right(head ex, (env, stdout))
      Nothing -> Left ('\'': xname ++ "' not found")
  )
-- Sym以外のアトムのときはそのまま
eval a@(Atm _) = return a


-- +
eval (op@(Atm (SYM "+")) :. Atm NIL) = return (int 0)
eval (op@(Atm (SYM "+")) :. x :. xs) = do
  Atm (NUM a) <- eval x
  Atm (NUM b) <- eval(op :. xs)
  return (Atm . NUM $ a + b)

-- *
eval (op@(Atm (SYM "*")) :. Atm NIL) = return (int 1)
eval (op@(Atm (SYM "*")) :. x :. xs) = do
  Atm (NUM a) <- eval x
  Atm (NUM b) <- eval(op :. xs)
  return (Atm . NUM $ a * b)

-- list -- リストを作る
eval ((Atm (SYM "list")) :. Atm NIL) = return nil
eval (op@(Atm (SYM "list")) :. x :. xs) = do
  a <- eval x
  b <- eval(op :. xs)
  return (a :. b)

-- define
eval(Atm (SYM "define") :. xsym@(Atm (SYM xname)) :. xs :. Atm NIL) = StateT (\env ->
    let s = runStateT (eval xs) env
    in case s of
      Right (x, (newEnv, stdout)) -> Right (xsym, (M.insertWith (++) xname [x] newEnv, stdout))
      _                 -> s
  )
-- if
eval(Atm (SYM "if") :. cond :. t :. f :. Atm NIL) = do
  c <- eval cond
  return $ if c == Atm T then t else f
-- display
eval(Atm (SYM "display") :. ex :. Atm NIL) = do
  c <- eval ex
  (env, stdout) <- get
  let st = case c of
            Atm (STR s) -> s
            _           -> show c
  put (env, stdout ++ st)
  return $ Atm UNDEF

-- lambda
eval(Atm (SYM "lambda") :. params :. content :. Atm NIL) = do
  return $ Atm (Lam params content) -- paramsがSYMだけで構成されているかはここでは確かめない

-- quote
eval(Atm (SYM "quote") :. list :. Atm NIL) = return list

-- car
eval(Atm (SYM "car") :. list :. Atm NIL) = do
  (car :. cdr) <- eval list -- TODO 取れない時がある
  return car

-- cdr
eval(Atm (SYM "cdr") :. list :. Atm NIL) = do
  (car :. cdr) <- eval list
  return cdr

-- cons
eval(Atm (SYM "cons") :. x :. xs :. Atm NIL) = do
  car <- eval x
  cdr <- eval xs
  return $ car :. cdr

-- equal?
eval(Atm (SYM "equal?") :. a :. b :. Atm NIL) = do
  x <- eval a
  y <- eval b
  return $ if x == y then t else f

-- pair? list? null?はhttp://d.hatena.ne.jp/satosystems/20100911/1284228783を参考にした

-- pair?
eval(Atm (SYM "pair?") :. a :. Atm NIL) = do
  x <- eval a
  return $ if x == Atm NIL then f else t

-- null?
eval(sym@(Atm (SYM "list?")) :. a :. Atm NIL) = do
  x <- eval a
  case x of
    Atm NIL       -> return t
    Atm _         -> return f
    _ :. xs       -> eval $ sym :. xs :. nil

eval(Atm (SYM "null?") :. xs :. nil) = do
  x <- eval xs
  return $ if x == nil then t else f

-- 適用する（プリミティブな関数以外はここで適用される）
eval(sym@(Atm (SYM lname)) :. args) = do
  (Atm (Lam params content)) <- eval sym -- TODO ラムダでない時にエラーをだすべき
  -- 引数に値をいれる
  pushParamsEnv params args
  e <- eval content
  -- 引数を環境から除く
  popParamsEnv params
  return e

  where
    -- 引数をEnvに入れる
    pushParamsEnv (Atm NIL) (Atm NIL)             = return (Atm UNDEF)
    pushParamsEnv ((Atm (SYM p)) :. ps) (a :. as) = do
      e             <- eval a
      (env, stdout) <- get
      put (M.insertWith (++) p [e] env, stdout)
      pushParamsEnv ps as

    -- 引数をEnvから外す
    popParamsEnv :: SExp -> StateT (Env, Stdout) (Either String) SExp
    popParamsEnv (Atm NIL)             = return (Atm UNDEF)
    popParamsEnv (Atm (SYM p) :. ps) = do
      (env, stdout) <- get
      let f x = Just $ tail x
      put (M.update f p env, stdout)
      popParamsEnv ps

---- END OF EVAL
eval ex = StateT (\_ -> Left (show ex ++ " not evaled"))

-- 'ex
quote ex = sym "quote" :. ex :. nil

test1_show = do
  let s1 = int 1
      s2 = (sym "define" :. sym "x" :. int 1 :. Atm NIL )
  print s1
  print s2

test2_eval = do
  let
    -- 1
    s1 = int 1
    -- (define x 1)
    s2 = (sym "define" :. sym "x" :. int 1 :. Atm NIL )
    -- (+ 1 2)
    s3 = (sym "+" :. int 1  :. int 2 :. nil)
    -- (+ 1 2 3 4 5)
    s4 = (sym "+" :. int 1  :. int 2 :. int 3 :. int 4 :. int 5 :. nil)
    -- (* 1 2 3 4 5)
    s5 = (sym "*" :. int 1  :. int 2 :. int 3 :. int 4 :. int 5 :. nil)
    -- (if () 2 "hello")
    s6 = (sym "if" :. nil :. int 2 :. str "hello" :. nil)
    -- (lambda (x) x)
    s7 = (sym "lambda" :. (sym "x" :. nil ) :. sym "x" :. nil)
    -- '(1 2 3)
    s8 = (sym "quote" :. (int 1 :. int 2 :. int 3 :. nil) :. nil)
    -- (1 2 3)
    s9 = (int 1 :. int 2 :. int 3 :. nil)
    -- (car '(1 2 3))
    s10 = (sym "car" :. (sym "quote" :. (int 1 :. int 2 :. int 3 :. nil) :. nil) :. nil)
    -- (crd '(1 2 3))
    s11 = (sym "cdr" :. (sym "quote" :. (int 1 :. int 2 :. int 3 :. nil) :. nil) :. nil)
    -- (list 1 2 3)
    s12 = (sym "list" :. int 1 :. int 2 :. int 3 :. nil)


  print $ runStateT (eval s1) (M.empty, "")
  print $ runStateT (eval s2) (M.empty, "")
  print $ runStateT (eval s3) (M.empty, "")
  print $ runStateT (eval s4) (M.empty, "")
  print $ runStateT (eval s5) (M.empty, "")
  print $ runStateT (eval s6) (M.empty, "")
  print $ runStateT (eval s7) (M.empty, "")
  print $ runStateT (eval s8) (M.empty, "")
  print $ runStateT (eval s9) (M.empty, "") -- これは失敗すべき
  print $ runStateT (eval s10) (M.empty, "")
  print $ runStateT (eval s11) (M.empty, "")
  print $ runStateT (eval s12) (M.empty, "")



-- 複数行のS式、つまりSchemeのプログラムを実行する
test3_eval = do
  let program =
        int 1 :
        (sym "define" :. sym "x" :. int 1 :. Atm NIL ) :
        (sym "define" :. sym "y" :. int 2 :. Atm NIL ) :
        (sym "+" :. int 1  :. int 2 :. nil) :
        -- (+ 1 2 3 4 5)
        (sym "+" :. int 1  :. int 2 :. int 3 :. int 4 :. int 5 :. nil) :
        -- (* 1 2 3 4 5)
        (sym "*" :. int 1  :. int 2 :. int 3 :. int 4 :. int 5 :. nil) :
        -- (if () 2 "hello")
        (sym "if" :. nil :. int 2 :. str "hello" :. nil) :
        -- (display x)
        (sym "display" :. sym "x" :. nil) :
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (display (+ 1 2))
        (sym "display" :. (sym "+" :. int 1  :. int 2 :. nil) :. nil) :
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (display (+ x y))
        (sym "display" :. (sym "+" :. sym "x"  :. sym "y" :. nil) :. nil) :
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (display (+ 1 10.0))
        (sym "display" :. (sym "+" :. int 1  :. real 10 :. nil) :. nil) :
        -- (define id (lambda (x) x))
        (sym "define" :. sym ("id") :. (sym "lambda" :. (sym "x" :. nil ) :. sym "x" :. nil) :. nil) :
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (id 10)
        (sym "display" :. (sym "id" :. int 10 :. nil) :. nil):
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (display (+ x y))
        (sym "display" :. (sym "+" :. sym "x"  :. sym "y" :. nil) :. nil) :
        -- (display "\n")
        (sym "display" :. str "\n" :. nil):
        -- (define a (car '(1 2 3)) )
        (sym "define" :. sym "a" :. (sym "car" :. (sym "quote" :. (int 1 :. int 2 :. int 3 :. nil) :. nil) :. nil) :. nil):
        -- (define b (crd '(1 2 3)))
        (sym "define" :. sym "b" :. (sym "cdr" :. (sym "quote" :. (int 1 :. int 2 :. int 3 :. nil) :. nil) :. nil) :. nil) :
        -- (display (cons a b))
        (sym "display" :. (sym "cons" :. sym "a" :. sym "b" :. nil) :. nil):
        []


  mapM print program

  -- programの実行
  let res = runStateT (forM program eval) (M.empty, "")
  case res of
    Right (evaledExps, (env, stdout)) -> do
      printf "Evaled Exps: %s\n" (show evaledExps)
      printf "Env: %s\n" (show env)
      printf "Stdout: \n%s\n" stdout
    Left cause                       -> do
      putStrLn ("failed: " ++ cause)
  return ()

-- null?の挙動確認用
test4_null = do
  let
    -- (null? ())
    s1 = sym "null?" :. sym "()" :. nil
    -- (null? (list))
    s2 = sym "null?" :. (sym "list" :. nil) :. nil
    -- (null? '(a))
    s3 = sym "null?" :. (sym "quote" :. (sym "a" :. nil) :. nil) :. nil
    -- (null? 'a)
    s4 = sym "null?" :. (sym "quote" :. sym "a" :. nil) :. nil

  print $ runStateT (eval s1) (M.empty, "")
  print $ runStateT (eval s2) (M.empty, "")
  print $ runStateT (eval s3) (M.empty, "")
  print $ runStateT (eval s4) (M.empty, "")


test5_pair = do
  let
    s1 = sym "pair?" :. sym "()" :. nil
    s2 = sym "pair?" :. quote (sym "a" :. nil) :. nil


  print $ runStateT (eval s1) (M.empty, "")
  print $ runStateT (eval s2) (M.empty, "")
  -- print $ runStateT (eval s3) (M.empty, "")
  -- print $ runStateT (eval s4) (M.empty, "")

main :: IO ()
main = test3_eval
