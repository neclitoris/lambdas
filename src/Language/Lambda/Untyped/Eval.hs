module Language.Lambda.Untyped.Eval
  ( Alpha
  , applyAlpha
  , reduce'
  , reduce
  , normalize'
  , normalize
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Functor.Foldable (cata, para, ana, hylo, project)
import Data.Maybe (fromJust)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

import Language.Lambda.Untyped.AST

data Alpha = Alpha Text MarkedAST

genName :: [Text] -> Text -> Text
genName used cur =
  case filter (`notElem` used) $ cur : [cur <> Text.pack (show i) | i <- [0 ..]] of
    name : _ -> name
    []       -> error "Could not generate unique name (should not happen)"

applyAlpha :: Alpha -> MarkedAST -> MarkedAST
applyAlpha (Alpha var t') = hylo app collect
  where
    app :: MarkedF ASTF MarkedAST -> MarkedAST
    app (MarkedF _ _ (VarF v))
      | v == var = t'
      | otherwise = markStep $ VarF v
    app (MarkedF _ _ x) = markStep x

    collect :: MarkedAST -> MarkedF ASTF MarkedAST
    collect a@(LamM b f v x)
      | v `elem` freeVars || v == var =
        MarkedF b f $ LamF v' $ applyAlpha (Alpha v (mark $ Var v')) x -- This is bad. TODO.
      | otherwise = project a
        where v' = genName (f `List.union` freeVars `List.union` [var]) v
    collect a = project a

    freeVars = let Marked _ f _ = t' in f

reduce' :: MarkedAST -> Maybe MarkedAST
reduce' = para \case
  (MarkedF _ _ (AppF (LamM _ _ v y, _) (x, _))) ->
    Just $ applyAlpha (Alpha v x) y
  (MarkedF _ _ (VarF v)) ->
    Nothing
  (MarkedF _ _ (AppF (_, Just x) (y, _))) ->
    Just $ markStep $ AppF x y
  (MarkedF _ _ (AppF (x, _) (_, Just y))) ->
    Just $ markStep $ AppF x y
  (MarkedF _ _ (AppF (_, Nothing) (_, Nothing))) ->
    Nothing
  (MarkedF _ _ (LamF v (_, x))) ->
    fmap markStep (LamF v <$> x)

reduce = fmap unmark . reduce' . mark

normalize' :: MarkedAST -> MarkedAST
normalize' x = maybe x normalize' (reduce' x)

normalize = unmark . normalize' . mark
