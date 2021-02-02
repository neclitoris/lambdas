module Language.Lambda.Untyped.Eval
  ( Alpha
  , applyAlpha
  , reduce'
  , reduce
  ) where

import Data.Functor.Foldable
import Data.List
import Data.Text (Text, pack)

import Language.Lambda.Untyped.AST

data Alpha = Alpha Text MarkedAST

genName :: [Text] -> Text -> Text
genName used cur =
  case filter (`notElem` used) $ cur : [cur <> pack (show i) | i <- [0 ..]] of
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
        where v' = genName (f `union` freeVars `union` [var]) v
    collect a = project a

    freeVars = let Marked _ f _ = t' in f

reduce' :: MarkedAST -> MarkedAST
reduce' = ana \case
  (AppM _ _ (LamM _ _ v y) x) ->
    project $ applyAlpha (Alpha v x) y
  any ->
    project any

reduce = unmark . reduce' . mark

