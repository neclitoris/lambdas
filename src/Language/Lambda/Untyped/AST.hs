{-# LANGUAGE TemplateHaskell #-}

module Language.Lambda.Untyped.AST
  ( ASTF(..)
  , AST
  , ASTLike(..)
  , Marked
  , MarkedF
  , MarkedAST
  , pattern Var
  , pattern App
  , pattern Lam
  , pattern MarkedF
  , pattern Marked
  , pattern VarM
  , pattern AppM
  , pattern LamM
  , mark
  , markStep
  , unmark
  ) where

import Data.Deriving
  ( deriveEq1
  , deriveFoldable
  , deriveOrd1
  , deriveShow1
  , deriveTraversable
  )
import Data.Fix (Fix(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Foldable (cata)
import Data.List qualified as List
import Data.Text (Text)
import GHC.Generics (Generic)

data ASTF a
  = VarF Text
  | AppF a a
  | LamF Text a
  deriving (Eq, Show, Generic, Functor)

data MarkedE t = MarkedE
  { bound :: [Text],
    free :: [Text],
    term :: t
  }
  deriving (Eq, Show, Generic, Functor)

$(deriveEq1 ''ASTF)
$(deriveOrd1 ''ASTF)
$(deriveShow1 ''ASTF)
$(deriveFoldable ''ASTF)
$(deriveTraversable ''ASTF)

$(deriveEq1 ''MarkedE)
$(deriveOrd1 ''MarkedE)
$(deriveShow1 ''MarkedE)
$(deriveFoldable ''MarkedE)
$(deriveTraversable ''MarkedE)

type AST = Fix ASTF

type MarkedF a = Compose MarkedE a

type Marked a = Fix (Compose MarkedE a)

type MarkedAST = Marked ASTF

pattern Var x = Fix (VarF x)

pattern App f x = Fix (AppF f x)

pattern Lam v x = Fix (LamF v x)

pattern MarkedF b f t = Compose (MarkedE b f t)

pattern Marked b f t = Fix (Compose (MarkedE b f t))

pattern VarM b f x = Marked b f (VarF x)

pattern AppM b f x y = Marked b f (AppF x y)

pattern LamM b f x y = Marked b f (LamF x y)

mark :: AST -> MarkedAST
mark = cata markStep

markStep :: ASTF MarkedAST -> MarkedAST
markStep (VarF x) = Marked [] [x] (VarF x)
markStep (AppF x@(Marked b1 f1 _) y@(Marked b2 f2 _)) =
  Marked (List.union b1 b2) (List.union f1 f2) (AppF x y)
markStep (LamF v x@(Marked b f _)) =
  Marked (List.union b [v]) (List.delete v f) (LamF v x)

unmark :: MarkedAST -> AST
unmark = cata (Fix . term . getCompose)

class ASTLike a where
  varC :: Text -> a
  appC :: a -> a -> a
  lamC :: Text -> a -> a

instance ASTLike AST where
  varC = Var
  appC = App
  lamC = Lam
