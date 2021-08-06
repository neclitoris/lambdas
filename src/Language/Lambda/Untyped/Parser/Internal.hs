module Language.Lambda.Untyped.Parser.Internal
  ( Parser
  , var
  , app
  , lam
  , subexpr
  , expr
  ) where

import Data.Functor ((<$>))
import Data.Void (Void)
import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Text.Megaparsec (Parsec, (<|>))
import Text.Megaparsec qualified as MPC
import Text.Megaparsec.Char qualified as MPC
import Data.Text (Text, pack)

import Language.Lambda.Untyped.AST

type Parser = Parsec Void Text

var :: ASTLike a => Parser a
var = varC . pack <$> MPC.some (MPC.alphaNumChar <|> MPC.char '\'')

app :: ASTLike a => Parser (a -> a -> a)
app = pure appC

lam :: ASTLike a => Parser a -> Parser a
lam expr = do
  MPC.char '\\'
  MPC.space
  v <- MPC.some MPC.alphaNumChar
  MPC.space
  MPC.char '.'
  MPC.space
  lamC (pack v) <$> expr

subexpr :: ASTLike a => Parser a
subexpr = MPC.try $ do
  MPC.space
  choice [var, lam expr, MPC.char '(' *> expr <* MPC.char ')']

expr :: ASTLike a => Parser a
expr = makeExprParser subexpr [[InfixL app]]
