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
import Grammar.Parsing (parse) as WithGrammar
import Parsing (Parser, runParser, ParseError(..), Position(..))


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" do
    let
      grm = parsesGrammar
      grmfile = parsesGrammarFile
      grmfail = failsToParseGrammarWithError
      withgrm = parsesWithGivenGrammarAs
      withgrmfail = failsToParseWithGivenGrammarWithError
      withgrmfile = parsesWithGrammarFromFile
      mkerr = mkParseError

    describe "parsing grammars (inline)" do
      it "should parse simple grammar (string)" $
        grm "main :- \"foobar\".\n" "main :- \"foobar\".\n"
      it "should parse simple grammar (char)" $
        grm "main :- 'x'.\n" "main :- 'x'.\n"
      it "should parse simple grammar (not char)" $
        grm "main :- ^'x'.\n" "main :- ^'x'.\n"
      it "should parse simple grammar (range)" $
        grm "main :- [a-z]." "main :- [a-z].\n"
      it "should parse simple grammar (range 2)" $
        grm "main :- [0-9]." "main :- [0-9].\n"
      it "should parse simple grammar (repsep)" $
        grm "main :- repSep(., '?')." "main :- repSep(.,'?').\n"
      it "should parse simple grammar (repsep 2)" $
        grm "main :- repSep([foo,bar,buz], (ws|ww|\"\\n\"))." "main :- repSep([foo,bar,buz],(ws|ww|\"\\n\")).\n"
      it "should parse simple grammar (repsep alt)" $
        grm "main :- / foo+ // ws /." "main :- repSep(foo,ws).\n"
      it "should parse simple grammar (repsep alt 2)" $
        grm "main :- /[foo,bar,buz]+ // (ws|ww|\"\\n\")/." "main :- repSep([foo,bar,buz],(ws|ww|\"\\n\")).\n"
      it "should parse simple grammar (sequence 1)" $
        grm "main :- [.,a,b]." "main :- [.,a,b].\n"
      it "should parse simple grammar (sequence 2)" $
        grm "main :- [.,'a','b']." "main :- [.,'a','b'].\n"
      it "should parse simple grammar (choice 1)" $
        grm "main :- (.|a|b)." "main :- (.|a|b).\n"
      it "should parse simple grammar (choice 2)" $
        grm "main :- (.|'a'|'b')." "main :- (.|'a'|'b').\n"
      it "should parse simple grammar (rule)" $
        grm "main :- rule." "main :- rule.\n"
      it "should parse simple grammar (rule with capture)" $
        grm "main :- foo:rule." "main :- foo:rule.\n"
      it "should parse grammar with comments" $
        grm "# comment\nmain :- foo.\n" "main :- foo.\n"
      it "should parse weird grammar with spaces" $
        grm "main    :-    [ . , . ]    ." "main :- [.,.].\n"
      it "should parse weird grammar ending with EOL" $
        grm "main    :-    [ . , . ]    .\n" "main :- [.,.].\n"
      it "should parse weird grammar ending with spaces and EOL" $
        grm "main    :-    [ . , . ]    . \n" "main :- [.,.].\n"
      it "should parse simple grammar w/o main rule" $
        grm "some :- .." "main :- ???.\nsome :- .."
      it "should parse simple grammar w/o main rule, v. 2" $
        grm "some :- 'x'." "main :- ???.\nsome :- 'x'."
      it "should parse simple grammar with several rules" $
        grm "main :- some.\nsome :- .." "main :- some.\nsome :- .."
      it "should parse simple grammar with several rules end empty lines" $
        grm "main :- some.\n\nsome :- .." "main :- some.\nsome :- .."
      it "should parse grammar with escaped characters in chars" $
        grm
          "main :- '\n'."
          "main :- '\n'.\n"
      it "should parse grammar with escaped characters in negated chars" $
        grm
          "main :- ^'\n'."
          "main :- ^'\n'.\n"
      it "should parse grammar with escaped quote in chars" $
        grm
          """char :- '\''."""
          "main :- ???.\nchar :- '\\''."
      it "should parse grammar with escaped characters in strings" $
        grm
          "main :- \"'\"."
          "main :- \"'\".\n"
      it "should parse grammar with escaped quote in strings" $
        grm
          """string :- "\""."""
          "main :- ???.\nstring :- \"\\\"\"."
      it "should parse complex grammar with escaped quote in strings" $
        grm
          """string :- ["\"", repSep(stringChar, ""), "\""]."""
          "main :- ???.\nstring :- [\"\\\"\",repSep(stringChar,\"\"),\"\\\"\"]."
      pending' "should parse simple grammar with lines before main" $
        grm "\n\n\nmain :- \"foo\"." "main :- \"foo\""
      pending' "properly fails when there's no dot in the end of the rule" $
        grmfail "main :- some" $ mkerr "no dot in the end of the rule" 1 1 0
      it "collects all rules" $
        grm
          """main :- repSep(fo,',').
          fo :- ('f'|'o').
          """
          "main :- repSep(fo,',').\nfo :- ('f'|'o')."

    describe "grammars from files" $ do
      it "parses `blocks.grammar`" $
        grmfile "blocks"
      it "parses `fp.grammar`" $
        grmfile "fp"
      it "parses `contracts.grammar`" $
        grmfile "contracts"
      it "parses `plainText.grammar`" $
        grmfile "plainText"
      it "parses `json.grammar`" $
        grmfile "json"
      pending' "parses `test`" $
        grmfile "test"

    describe "parsing with grammars" do
      pending' "failing to parse" $
        withgrm
          "?"
          """main :- "foo"."""
          "-"
      it "parsing strings" $
        withgrm
          "foo"
          """main :- "foo"."""
          """( 0 <main> text 0-3 )"""
      it "parsing chars" $
        withgrm
          "f"
          "main :- 'f'."
          """( 0 <main> char 0-1 )"""
      it "parsing negated chars" $
        withgrm
          "o"
          "main :- ^'f'."
          """( 0 <main> not-char 0-1 )"""
      it "parsing char sequences" $
        withgrm
          "foo"
          "main :- ['f','o','o']."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 char 0-1 ) : ( 0 seq:1 char 1-2 ) : ( 0 seq:2 char 2-3 ) )"
      it "parsing any-char sequences" $
        withgrm
          "foo"
          "main :- [.,.,.]."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 any 0-1 ) : ( 0 seq:1 any 1-2 ) : ( 0 seq:2 any 2-3 ) )"
      it "parsing char's and rule sequences" $
        withgrm "foo"
          """main :- [f,o,o].
          f :- 'f'.
          o :- 'o'.
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing sequences with empty items" $
        withgrm "foo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:nothing text 1-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:nothing text 2-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing char choice" $
        withgrm
          "o"
          "main :- ('f'|'o'|'o')."
          "( 0 <main> choice 0-1 | ( 0 ch:1 char 0-1 ) )"
      it "parsing rep/sep" $
        withgrm "f,o,o"
          """main :- repSep(fo,',').
          fo :- ('f'|'o').
          """
          "( 0 <main> repsep 0-5 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) : ( 0 rule:fo choice 4-5 | ( 0 ch:1 char 4-5 ) ) )"
      it "parsing rep/sep 2" $
        withgrm "foo"
          """main :- repSep(fo,"").
          fo :- ('f'|'o').
          """
          "( 0 <main> repsep 0-3 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 rule:fo choice 1-2 | ( 0 ch:1 char 1-2 ) ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) )"
      it "parsing rep/sep when empty" $
        withgrm ""
          """main :- repSep(" ","\n")."""
          "( 0 <main> repsep 0-0 | ∅ )"
      it "parsing rep/sep when empty 2" $
        withgrm "\n"
          """main :- repSep(" ","\n")."""
          "( 0 <main> repsep 0-0 | ∅ )"
      {- pending' "parsing rep/sep when empty 3" $
        withgrm ""
          """main :- repSep("","")."""
          "( 0 <main> repsep 0-0 | ∅)" -}
      it "parsing rep/sep when empty 4" $
        withgrm ""
          """main :- repSep("a","b")."""
          "( 0 <main> repsep 0-0 | ∅ )"
      it "parsing rep/sep when empty 5" $
        withgrm ""
          """main :- repSep("a","")."""
          "( 0 <main> repsep 0-0 | ∅ )"
      it "parsing rep/sep when empty 6" $
        withgrm ""
          """main :- repSep("a","")."""
          "( 0 <main> repsep 0-0 | ∅ )"
      it "capture works" $
        withgrm "[a-z]"
          """main :- ['[',from:char,'-',to:char,']'].
          char :- ('a'|'z').
          """
          "( 0 <main> seqnc 0-5 | ( 0 seq:0 char 0-1 ) : ( 0 rule:from choice 1-2 | ( 0 ch:0 char 1-2 ) ) : ( 0 seq:2 char 2-3 ) : ( 0 rule:to choice 3-4 | ( 0 ch:1 char 3-4 ) ) : ( 0 seq:4 char 4-5 ) )"
      it "plain text grammar works" $
        withgrm "foobar"
          """main :- repSep(., "")."""
          "( 0 <main> repsep 0-6 | ( 0 rep any 0-1 ) : ( 0 rep any 1-2 ) : ( 0 rep any 2-3 ) : ( 0 rep any 3-4 ) : ( 0 rep any 4-5 ) : ( 0 rep any 5-6 ) )"
      it "parsing newlines (as chars)" $
        withgrm "\n"
          """main :- '\n'."""
          "( 0 <main> char 0-1 )"
      it "parsing newlines (as strings)" $
        withgrm "\n"
          """main :- "\n"."""
          "( 0 <main> text 0-1 )"
      it "parsing char ranges" $
        withgrm "234"
          "main :- [digit,digit,digit].\ndigit :- [0-9]."
          "( 0 <main> seqnc 0-3 | ( 0 rule:digit char-range 0-1 ) : ( 0 rule:digit char-range 1-2 ) : ( 0 rule:digit char-range 2-3 ) )"
      it "num is not parsed as alpha" $
        withgrmfail "2"
          "main :- [a-z]."
          $ mkerr "'2' is not from range a-z" 1 2 1
      it "parsing char ranges in sequences" $
        withgrm "2"
          "main :- [[0-9]]."
          "( 0 <main> seqnc 0-1 | ( 0 seq:0 char-range 0-1 ) )"
      it "parsing char ranges in sequences 2" $
        withgrm "2"
          "main :- [[0-9],repSep([0-9], '')]."
          ""
      it "parses `blocks`" $
        withgrmfile "blocks"
      it "parses `grammar`" $
        withgrmfile "grammar"
      it "parses `json`" $
        withgrmfile "json"
      it "parses `plainText`" $
        withgrmfile "plainText"
      it "parses `fp`" $
        withgrmfile "fp"
      it "parses `test`" $
        withgrmfile "test"



parsesGrammar ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String → m Unit
parsesGrammar grammarStr expectation =
  (show <$> runParser grammarStr Grammar.parser) `shouldEqual` (Right expectation)


parsesGrammarFile ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m ⇒ String → m Unit
parsesGrammarFile fileName = do
  grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
  expectation <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar.expected"
  let eGrammar =  runParser grammarStr Grammar.parser
  liftEffect $ writeTextFile Encoding.UTF8 ("./test/grammars/" <> fileName <> ".grammar.result") $ reportE eGrammar
  (show <$> eGrammar) `shouldEqual` (Right expectation)


failsToParseGrammarWithError ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → ParseError → m Unit
failsToParseGrammarWithError grammarStr error =
  (show <$> runParser grammarStr Grammar.parser) `shouldEqual` (Left error)


mkParseError :: String -> Int -> Int -> Int -> ParseError
mkParseError err line column index = ParseError err $ Position { line, column, index }


parsesWithGivenGrammarAs :: ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String -> String → m Unit
parsesWithGivenGrammarAs str grammarStr expectation =
  let
    eGrammar = runParser grammarStr Grammar.parser
    buildAst grammar = WithGrammar.parse grammar (const 0) str
  in (map show <$> buildAst =<< eGrammar) `shouldEqual` (Right expectation)


failsToParseWithGivenGrammarWithError ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String -> String → ParseError → m Unit
failsToParseWithGivenGrammarWithError str grammarStr error =
  let
    eGrammar = runParser grammarStr Grammar.parser
    buildAst grammar = WithGrammar.parse grammar (const 0) str
  in (map show <$> buildAst =<< eGrammar) `shouldEqual` (Left error)


parsesWithGrammarFromFile :: ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m => String -> m Unit
parsesWithGrammarFromFile fileName = do
    grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
    sourceStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src"
    expectationStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src.expected"
    let
      eGrammar =  runParser grammarStr Grammar.parser
      buildAst grammar = WithGrammar.parse grammar (const 0) sourceStr
      eAST = buildAst =<< eGrammar
    liftEffect $ writeTextFile Encoding.UTF8 ("./test/sources/" <> fileName <> ".src.result") $ reportE eAST
    (show <$> eAST) `shouldEqual` (Right expectationStr)


reportE :: forall a err. Show err => Show a => Either err a -> String
reportE = case _ of
    Left error -> "error::" <> show error
    Right grammar -> show grammar