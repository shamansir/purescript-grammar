module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


import Grammar.Parser (parser) as Grammar
import Parsing (Parser, runParser, ParseError)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" do
    describe "Grammar" do
      it "should parse simple grammar (string)" $
        g "main :- \"foobar\".\n" "main :- \"foobar\".\n"
      it "should parse simple grammar (char)" $
        g "main :- 'x'.\n" "main :- 'x'.\n"
      it "should parse simple grammar (not char)" $
        g "main :- ^'x'.\n" "main :- ^'x'.\n"
      it "should parse simple grammar (range)" $
        g "main :- [a-z]." "main :- ['a'-'z'].\n"
      it "should parse simple grammar (range 2)" $
        g "main :- [0-9]." "main :- ['0'-'9'].\n"
      it "should parse simple grammar (repsep)" $
        g "main :- repSep(., '?')." "main :- repSep(.,'?').\n"
      it "should parse simple grammar (repsep 2)" $
        g "main :- repSep([foo,bar,buz], (ws|ww|\"\\n\"))." "main :- repSep([foo,bar,buz],(ws|ww|\"\\n\")).\n"
      it "should parse simple grammar (repsep alt)" $
        g "main :- +[foo,bar,buz] // (ws|ww|\"\\n\")." "main :- repSep([foo,bar,buz],(ws|ww|\"\\n\")).\n"
      it "should parse simple grammar (sequence 1)" $
        g "main :- [.,a,b]." "main :- [.,a,b].\n"
      it "should parse simple grammar (sequence 2)" $
        g "main :- [.,'a','b']." "main :- [.,'a','b'].\n"
      it "should parse simple grammar (choice 1)" $
        g "main :- (.|a|b)." "main :- (.|a|b).\n"
      it "should parse simple grammar (choice 2)" $
        g "main :- (.|'a'|'b')." "main :- (.|'a'|'b').\n"
      it "should parse simple grammar (rule)" $
        g "main :- rule." "main :- rule.\n"
      it "should parse simple grammar (rule with capture)" $
        g "main :- foo:rule." "main :- foo:rule.\n"
      it "should parse grammar with comments" $
        g "# comment\nmain :- foo.\n" "main :- foo.\n"
      it "should parse weird grammar with spaces" $
        g "main    :-    [ . , . ]    ." "main :- [.,.].\n"
      it "should parse weird grammar ending with EOL" $
        g "main    :-    [ . , . ]    .\n" "main :- [.,.].\n"
      it "should parse weird grammar ending with spaces and EOL" $
        g "main    :-    [ . , . ]    . \n" "main :- [.,.].\n"
      it "should parse simple grammar w/o main rule" $
        g "some :- .." "main :- ???.\nsome :- .."
      it "should parse simple grammar w/o main rule, v. 2" $
        g "some :- 'x'." "main :- ???.\nsome :- 'x'."
      it "should parse simple grammar with several rules" $
        g "main :- some.\nsome :- .." "main :- some.\nsome :- .."
      it "should parse simple grammar with several rules end empty lines" $
        g "main :- some.\n\nsome :- .." "main :- some.\nsome :- .."



g ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String → m Unit
g str expectation =
  (show <$> runParser str Grammar.parser) `shouldEqual` (Right expectation)


gerr ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → ParseError → m Unit
gerr str error =
  (show <$> runParser str Grammar.parser) `shouldEqual` (Left error)
