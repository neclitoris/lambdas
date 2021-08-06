module Language.Lambda.Untyped.Parser
  ( parseExpr
  ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Text.Megaparsec qualified as MPC
import Text.Megaparsec.Char qualified as MPC

import Language.Lambda.Untyped.AST
import Language.Lambda.Untyped.Parser.Internal

parseExpr :: ASTLike a => Text -> Either String a
parseExpr =
  first MPC.errorBundlePretty . MPC.parse (expr <* MPC.space <* MPC.eof) ""
