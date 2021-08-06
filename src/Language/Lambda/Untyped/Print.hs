module Language.Lambda.Untyped.Print
  ( PP.pretty
  , showAST
  ) where

import Data.Functor.Foldable (para)
import Data.Text (Text)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Show (Show(..))

import Language.Lambda.Untyped.AST

instance PP.Pretty AST where
  pretty = para \case
    VarF v ->
      PP.pretty v
    AppF (_, x) (App{}, y) ->
      x <+> PP.parens y
    AppF (_, x) (_, y) ->
      x <+> y
    LamF v (_, x) ->
      PP.parens $ mconcat [PP.backslash, PP.pretty v, PP.dot] <+> x

instance {-# OVERLAPS #-} Show AST where
  showsPrec _ = PP.renderShowS . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

showAST :: AST -> Text
showAST = PP.renderStrict . PP.layoutSmart PP.defaultLayoutOptions . PP.pretty
