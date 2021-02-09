module Main where

import Data.Text.IO qualified as Text

import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print


main = do
  str <- Text.getLine
  case parseExpr str of
    Right expr -> Text.putStrLn $ showAST $ normalize expr
    Left err -> error err
