module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


import Grammar.Parser (parser) as Grammar
import Parsing (Parser, runParser)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" do
    describe "Grammar" do
      it "should parse simple grammar" do
        (show <$> runParser "main    :-    [ . , .]    ." Grammar.parser) `shouldEqual` (Right "---")
        -- (show <$> runParser "main :- repSep(.,\"\")." Grammar.parser) `shouldEqual` (Right "---")
