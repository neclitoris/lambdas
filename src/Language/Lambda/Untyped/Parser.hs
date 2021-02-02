module Language.Lambda.Untyped.Parser
  ( parseExpr
  , expr
  , MPC.errorBundlePretty
  ) where

import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec qualified as MPC
import Text.Megaparsec (Parsec, (<|>))
import Text.Megaparsec.Char qualified as MPC
import Data.Text (Text, pack)

import Language.Lambda.Untyped.AST

type Parser = Parsec Void Text

var :: Parser AST
var =
  Var . pack <$> (MPC.space *> MPC.some (MPC.alphaNumChar <|> MPC.char '\''))

app :: Parser (AST -> AST -> AST)
app = App <$ MPC.optional MPC.spaceChar

lam :: Parser AST -> Parser AST
lam expr = do
  MPC.space
  MPC.char '\\'
  MPC.space
  v <- MPC.some MPC.alphaNumChar
  MPC.space
  MPC.char '.'
  MPC.space
  Lam (pack v) <$> expr

subexpr :: Parser AST
subexpr = var <|> lam expr <|> MPC.char '(' *> expr <* MPC.char ')'

expr :: Parser AST
expr = makeExprParser subexpr [[InfixL app]]

parseExpr = MPC.parse (expr <* MPC.eof) ""
