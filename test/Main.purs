module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Class.Console (log)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it, itOnly, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..)) as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)

import Grammar.Parser (parser) as Grammar
import Grammar.With (parse) as WithGrammar
import Parsing (Parser, runParser, ParseError(..), Position(..))


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" do
    let
      g = parsesGrammar
      gfile = parsesGrammarFile
      pwith = parsesWithGivenGrammarAs
      gerr = failsToParseWithError
      perr = mkParseError

    describe "parsing grammars (inline)" do
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
        g "main :- / foo+ // ws /." "main :- repSep(foo,ws).\n"
      it "should parse simple grammar (repsep alt 2)" $
        g "main :- /[foo,bar,buz]+ // (ws|ww|\"\\n\")/." "main :- repSep([foo,bar,buz],(ws|ww|\"\\n\")).\n"
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
      it "should parse grammar with escaped characters in chars" $
        g
          "main :- '\n'."
          "main :- '\n'.\n"
      it "should parse grammar with escaped characters in negated chars" $
        g
          "main :- ^'\n'."
          "main :- ^'\n'.\n"
      it "should parse grammar with escaped quote in chars" $
        g
          """char :- '\''."""
          "main :- ???.\nchar :- '\\''."
      it "should parse grammar with escaped characters in strings" $
        g
          "main :- \"'\"."
          "main :- \"'\".\n"
      it "should parse grammar with escaped quote in strings" $
        g
          """string :- "\""."""
          "main :- ???.\nstring :- \"\\\"\"."
      it "should parse complex grammar with escaped quote in strings" $
        g
          """string :- ["\"", repSep(stringChar, ""), "\""]."""
          "main :- ???.\nstring :- [\"\\\"\",repSep(stringChar,\"\"),\"\\\"\"]."
      pending' "should parse simple grammar with lines before main" $
        g "\n\n\nmain :- \"foo\"." "main :- \"foo\""
      pending' "properly fails when there's no dot in the end of the rule" $
        gerr "main :- some" $ perr "no dot in the end of the rule" 1 1 0
      it "collects all rules" $
        g
          """main :- repSep(fo,',').
          fo :- ('f'|'o').
          """
          "main :- repSep(fo,',').\nfo :- ('f'|'o')."

    describe "grammars from files" $ do
      it "parses `blocks.grammar`" $
        gfile "blocks"
    describe "parsing with grammars" do
      pending' "failing to parse" $
        pwith
          "?"
          """main :- "foo"."""
          "-"
      it "parsing strings" $
        pwith
          "foo"
          """main :- "foo"."""
          """( 0 <main> text 0-3 )"""
      it "parsing chars" $
        pwith
          "f"
          "main :- 'f'."
          """( 0 <main> char 0-1 )"""
      it "parsing negated chars" $
        pwith
          "o"
          "main :- ^'f'."
          """( 0 <main> not-char 0-1 )"""
      it "parsing char sequences" $
        pwith
          "foo"
          "main :- ['f','o','o']."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 char 0-1 ) : ( 0 seq:1 char 1-2 ) : ( 0 seq:2 char 2-3 ) )"
      it "parsing any-char sequences" $
        pwith
          "foo"
          "main :- [.,.,.]."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 any 0-1 ) : ( 0 seq:1 any 1-2 ) : ( 0 seq:2 any 2-3 ) )"
      it "parsing char's and rule sequences" $
        pwith "foo"
          """main :- [f,o,o].
          f :- 'f'.
          o :- 'o'.
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing sequences with empty items" $
        pwith "foo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:nothing text 1-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:nothing text 2-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing char choice" $
        pwith
          "o"
          "main :- ('f'|'o'|'o')."
          "( 0 <main> choice 0-1 | ( 0 ch:1 char 0-1 ) )"
      it "parsing rep/sep" $
        pwith "f,o,o"
          """main :- repSep(fo,',').
          fo :- ('f'|'o').
          """
          "( 0 <main> repsep 0-5 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) : ( 0 rule:fo choice 4-5 | ( 0 ch:1 char 4-5 ) ) )"
      it "parsing rep/sep 2" $
        pwith "foo"
          """main :- repSep(fo,"").
          fo :- ('f'|'o').
          """
          "( 0 <main> repsep 0-3 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 rule:fo choice 1-2 | ( 0 ch:1 char 1-2 ) ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) )"
      it "capture works" $
        pwith "[a-z]"
          """main :- ['[',from:char,'-',to:char,']'].
          char :- ('a'|'z').
          """
          "( 0 <main> seqnc 0-5 | ( 0 seq:0 char 0-1 ) : ( 0 rule:from choice 1-2 | ( 0 ch:0 char 1-2 ) ) : ( 0 seq:2 char 2-3 ) : ( 0 rule:to choice 3-4 | ( 0 ch:1 char 3-4 ) ) : ( 0 seq:4 char 4-5 ) )"
      it "plain text grammar works" $
        pwith "foobar"
          """main :- repSep(., "")."""
          "( 0 <main> repsep 0-6 | ( 0 rep any 0-1 ) : ( 0 rep any 1-2 ) : ( 0 rep any 2-3 ) : ( 0 rep any 3-4 ) : ( 0 rep any 4-5 ) : ( 0 rep any 5-6 ) )"



parsesGrammar ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String → m Unit
parsesGrammar grammarStr expectation =
  (show <$> runParser grammarStr Grammar.parser) `shouldEqual` (Right expectation)


parsesGrammarFile ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m ⇒ String → m Unit
parsesGrammarFile fileName = do
  grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
  expectation <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar.compiled"
  let result =  runParser grammarStr Grammar.parser
  liftEffect $ writeTextFile Encoding.UTF8 ("./test/grammars/" <> fileName <> ".grammar.result") $ case result of
    Left error -> "error::" <> show error
    Right grammar -> show grammar
  (show <$> result) `shouldEqual` (Right expectation)


failsToParseWithError ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → ParseError → m Unit
failsToParseWithError grammarStr error =
  (show <$> runParser grammarStr Grammar.parser) `shouldEqual` (Left error)


mkParseError :: String -> Int -> Int -> Int -> ParseError
mkParseError err line column index = ParseError err $ Position { line, column, index }


parsesWithGivenGrammarAs :: ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String -> String → m Unit
parsesWithGivenGrammarAs str grammarStr expectation =
  let
    eGrammar = runParser grammarStr Grammar.parser
    ast grammar = WithGrammar.parse grammar (const 0) str
  in (map show <$> ast =<< eGrammar) `shouldEqual` (Right expectation)


parsesWithGrammarFromFileAs :: ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String -> String → m Unit
parsesWithGrammarFromFileAs str grammarStr expectation =
  let
    eGrammar = runParser grammarStr Grammar.parser
    ast grammar = WithGrammar.parse grammar (const 0) str
  in (map show <$> ast =<< eGrammar) `shouldEqual` (Right expectation)