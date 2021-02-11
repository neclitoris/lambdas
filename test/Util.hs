{-# LANGUAGE TemplateHaskell #-}
module Util
  ( render
  , matchPat
  )where

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Ppr qualified as TH
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

render :: PP.Doc a -> String
render = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions

matchPat :: TH.PatQ -> TH.ExpQ
matchPat p =
  [|
    \case
      $(p) ->
        pure ()
      any ->
        Tasty.assertBool
          (render $ PP.vsep
            [ "expected pattern"
            , PP.indent 2 $ PP.parens $ PP.pretty
              ($(p >>= TH.lift . TH.pprint) :: String)
            , "to match, got"
            , PP.indent 2 $ PP.parens $ PP.pretty any
            ])
          False |]
