module Main where

import Data.Text.IO qualified as Text
import Prettyprinter.Render.Text

import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print


main = do
  str <- getLine
  case parseExpr str of
    Right expr -> Text.putStrLn $ showAST $ reduce expr
    Left err -> error $ errorBundlePretty err
