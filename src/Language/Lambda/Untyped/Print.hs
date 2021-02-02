module Language.Lambda.Untyped.Print
  ( pretty
  , showAST
  ) where

import Data.Functor.Foldable
import Prettyprinter
import Prettyprinter.Render.Text
import Data.Text qualified as Text

import Language.Lambda.Untyped.AST

instance Pretty AST where
  pretty = para \case
    VarF v ->
      pretty v
    AppF (Lam{}, x) (App{}, y) ->
      parens x <+> parens y
    AppF (Lam{}, x) (_, y) ->
      parens x <+> y
    AppF (_, x) (App{}, y) ->
      x <+> parens y
    AppF (_, x) (_, y) ->
      x <+> y
    LamF v (_, x) ->
      mconcat [backslash, pretty v, dot] <+> x

showAST :: AST -> Text.Text
showAST = renderStrict . layoutSmart defaultLayoutOptions . pretty
