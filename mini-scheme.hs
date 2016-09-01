-- ./mini-scheme file.scmで実行できるインタプリタ

import           Control.Monad.State
import qualified Data.DList          as D
import qualified Data.Map            as M
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
          -- 実行する
          let res = runStateT (forM exps eval) (M.empty, D.fromList "")
          case res of
            Right (evaledExps, (env, stdout)) -> do
              -- print env
              -- 標準出力の部分を出力
              putStr (D.toList stdout)
            -- 実行の失敗
            Left cause                       -> do
              putStrLn ("failed: " ++ cause)
        -- パースの失敗
        Left cause -> print cause
    _            -> do
      putStrLn "Usaage: mini-scheme file.scm"
