{-# LANGUAGE TemplateHaskell #-}
module Language.Lambda.Untyped.TH
  ( lambda
  ) where

import Data.Text (Text, pack, unpack)
import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Lib qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as MPC
import Text.Megaparsec.Char qualified as MPC

import Language.Lambda.Untyped.AST
import Language.Lambda.Untyped.Parser.Internal

instance ASTLike TH.PatQ where
  varC v   = [p| Var $(TH.varP $ TH.mkName $ unpack v) |]
  appC x y = [p| App $(x) $(y) |]
  lamC v x = [p| Lam $(TH.varP $ TH.mkName $ unpack v) $(x) |]

instance ASTLike TH.ExpQ where
  varC v   = [| Var v |]
  appC x y = [| App $(x) $(y) |]
  lamC v x = [| Lam v $(x) |]


class ASTLike a => ASTEmbed a where
  subC :: String -> a

instance ASTEmbed TH.PatQ where
  subC = TH.varP . TH.mkName

instance ASTEmbed TH.ExpQ where
  subC = TH.dyn


embedded :: ASTEmbed a => Parser a
embedded = do
  MPC.char '{'
  name <- (:) <$> MPC.letterChar <*> MPC.many (MPC.alphaNumChar <|> MPC.char '\'')
  MPC.char '}'
  pure $ subC name

subexprTH :: ASTEmbed a => Parser a
subexprTH = MPC.try $ do
  MPC.space
  choice [embedded, var, lam exprTH, MPC.char '(' *> exprTH <* MPC.char ')']

exprTH :: ASTEmbed a => Parser a
exprTH = makeExprParser subexprTH [[InfixL app]]

parseExprTH :: ASTEmbed a => Text -> a
parseExprTH str =
  case MPC.parse (exprTH <* MPC.space <* MPC.eof) "" str of
    Right res -> res
    Left err  -> error $ MPC.errorBundlePretty err

lambda :: TH.QuasiQuoter
lambda = TH.QuasiQuoter
  { TH.quoteExp  = parseExprTH . pack
  , TH.quotePat  = parseExprTH . pack
  , TH.quoteType = notImplemented "type"
  , TH.quoteDec  = notImplemented "declaration"
  } where
    notImplemented what = error $ "Quasi quotes for " <> what <> " are not implemented"
