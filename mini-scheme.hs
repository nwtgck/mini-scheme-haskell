-- ./mini-scheme file.scmで実行できるインタプリタ

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.DList                 as D
import qualified Data.Map                   as M
import           Scheme
import           SchemeParser
import           System.Environment
import           Text.Parsec
import           Text.Printf

main :: IO ()
main = do
  -- コマンドライン引数から実行したいscmファイルを取得
  args <- getArgs
  case args of
    (filename:_) -> do
      -- ファイルの内容を読み取る
      sourceCode <- readFile filename
      -- パースする
      case parse miniScheme "" sourceCode of
        -- パースが成功した時
        Right exps -> do
          let printStdout :: SExp -> StateT (Env, Stdout) (EitherT String IO) SExp
              printStdout sexp = do
                evaled <- eval sexp

                -- Schemeの標準出力の内容を取得して表示
                (env, stdout) <- get
                liftIO $ putStr (D.toList stdout)
                -- 表示したので空文字列を入れる（標準出力のバッファてきなものになっている）
                put (env, D.fromList "")

                return evaled
          -- 実行する
          res <- runEitherT ( runStateT (forM exps printStdout) (M.empty, D.fromList "") )
          case res of
            Right (evaledExps, (env, stdout)) -> do
              return ()
            -- 実行の失敗
            Left cause                       -> do
              putStrLn ("failed: " ++ cause)
        -- パースの失敗
        Left cause -> print cause
    _            -> do
      putStrLn "Usaage: mini-scheme file.scm"
