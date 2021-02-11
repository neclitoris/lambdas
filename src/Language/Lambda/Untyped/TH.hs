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

newtype ASTPat = ASTPat { unASTPat :: TH.PatQ }
newtype ASTExp = ASTExp { unASTExp :: TH.ExpQ}

instance ASTLike ASTPat where
  varC v   = ASTPat [p| Var $(TH.varP $ TH.mkName $ unpack v) |]
  appC x y = ASTPat [p| App $(unASTPat x) $(unASTPat y) |]
  lamC v x = ASTPat [p| Lam $(TH.varP $ TH.mkName $ unpack v) $(unASTPat x) |]

instance ASTLike ASTExp where
  varC v   = ASTExp [| Var v |]
  appC x y = ASTExp [| App $(unASTExp x) $(unASTExp y) |]
  lamC v x = ASTExp [| Lam v $(unASTExp x) |]


class ASTLike a => ASTEmbed a where
  subC :: String -> a

instance ASTEmbed ASTPat where
  subC "_"  = ASTPat TH.wildP
  subC name = ASTPat $ TH.varP $ TH.mkName name

instance ASTEmbed ASTExp where
  subC "_"  = ASTExp $ fail "wildcards can't be used in expression context"
  subC name = ASTExp $ TH.dyn name


wildcard :: ASTEmbed a => Parser a
wildcard = subC . (:[]) <$> MPC.char '_'

embedded :: ASTEmbed a => Parser a
embedded = do
  MPC.char '{'
  name <- (:) <$> MPC.letterChar <*> MPC.many (MPC.alphaNumChar <|> MPC.char '\'')
  MPC.char '}'
  pure $ subC name

subexprTH :: ASTEmbed a => Parser a
subexprTH = MPC.try $ do
  MPC.space
  choice [ wildcard
         , embedded
         , var
         , lam exprTH
         , MPC.char '(' *> exprTH <* MPC.char ')'
         ]

exprTH :: ASTEmbed a => Parser a
exprTH = makeExprParser subexprTH [[InfixL app]]

parseExprTH :: ASTEmbed a => Text -> a
parseExprTH str =
  case MPC.parse (exprTH <* MPC.space <* MPC.eof) "" str of
    Right res -> res
    Left err  -> error $ MPC.errorBundlePretty err

lambda :: TH.QuasiQuoter
lambda = TH.QuasiQuoter
  { TH.quoteExp  = unASTExp . parseExprTH . pack
  , TH.quotePat  = unASTPat . parseExprTH . pack
  , TH.quoteType = notImplemented "type"
  , TH.quoteDec  = notImplemented "declaration"
  } where
    notImplemented what = error $ "Quasi quotes for " <> what <> " are not implemented"
