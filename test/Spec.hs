import Data.Bifunctor (first)
import Data.Text qualified as Text
import Control.Applicative
import Hedgehog hiding (defaultMain)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Language.Lambda.Untyped.AST qualified as AST
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print

tests :: TestTree
tests = testGroup "tests"
  [ props ]

props :: TestTree
props = testGroup "properties"
  [ testProperty "print-parse" prop_print_parse ]

genName :: (MonadGen m) => m Text.Text
genName = Gen.text (Range.exponential 1 8) Gen.lower

genAST :: (MonadGen m) => m AST.AST
genAST = Gen.recursive Gen.choice
  [ AST.Var <$> genName ]
  [ Gen.subtermM genAST (\x -> AST.Lam <$> genName <*> pure x)
  , Gen.subterm2 genAST genAST AST.App ]

prop_print_parse :: Property
prop_print_parse =
  property $ do
    expr <- forAll genAST
    expr' <- evalEither $ parseExpr (showAST expr)
    annotateShow (showAST expr)
    annotateShow (showAST expr')
    expr' === expr

main :: IO ()
main = defaultMain tests
