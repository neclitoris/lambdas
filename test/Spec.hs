{-# LANGUAGE TemplateHaskell #-}
import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Hedgehog hiding (defaultMain, Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Language.Lambda.Untyped.AST
import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print
import Language.Lambda.Untyped.TH

import Util

tests :: Tasty.TestTree
tests = Tasty.testGroup "tests"
  [ props
  , reduction
  ]

props :: Tasty.TestTree
props = Tasty.testGroup "properties"
  [ Tasty.testProperty "print-parse" prop_print_parse
  , Tasty.testProperty "normal form" prop_normal_form
  , Tasty.testProperty "fixpoint" prop_fixpoint
  ]

genName :: (MonadGen m) => m Text
genName = Gen.text (Range.exponential 1 8) Gen.lower

genAST :: (MonadGen m) => m AST
genAST = Gen.recursive Gen.choice
  [ Var <$> genName ]
  [ Gen.subtermM genAST (\x -> Lam <$> genName <*> pure x)
  , Gen.subterm2 genAST genAST App ]

prop_print_parse :: Property
prop_print_parse =
  property $ do
    expr <- forAll genAST
    expr' <- evalEither $ parseExpr (showAST expr)
    annotateShow (showAST expr)
    annotateShow (showAST expr')
    expr' === expr

prop_normal_form :: Property
prop_normal_form =
  property $ do
    expr <- forAll genAST
    annotateShow (showAST expr)
    expr' <- eval $ normalize expr
      -- TODO workaround for random fixpoint
    annotateShow (showAST expr')
    reduce expr' === Nothing

prop_fixpoint :: Property
prop_fixpoint =
  property $ do
    depth <- forAll $ Gen.int $ Range.linear 1 1000
    test expr depth
  where
    fix = [lambda| \f. (\x. f (x x)) (\x. f (x x)) |]
    expr = (reduce <=< reduce) [lambda| {fix} f |]

    test _ 0 = success
    test (Just [lambda| f {e} |]) n = test (reduce e) (n - 1)
    test _ _ = failure

reduction :: Tasty.TestTree
reduction = Tasty.testGroup "reduction correctness"
  [ Tasty.testCase "single reduction" $
      $(matchPat [p| Just [lambda| y |] |]) $
        reduce [lambda| (\x. x) y |]

  , Tasty.testCase "no reduction" $
      $(matchPat [p| Nothing |]) $
        reduce [lambda| (\x. x) |]

  , Tasty.testCase "normal order" $
      $(matchPat [p| Just [lambda| z |] |]) $
        reduce [lambda| (\f. z) ((\x. x x) (\x. x x)) |]
  ]

main :: IO ()
main = Tasty.defaultMain tests
