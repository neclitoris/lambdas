import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog hiding (defaultMain, Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Language.Lambda.Untyped.AST
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print

tests :: Tasty.TestTree
tests = Tasty.testGroup "tests"
  [ props ]

props :: Tasty.TestTree
props = Tasty.testGroup "properties"
  [ Tasty.testProperty "print-parse" prop_print_parse ]

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

main :: IO ()
main = Tasty.defaultMain tests
