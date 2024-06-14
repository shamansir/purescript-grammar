module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Class.Console (log)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)

import Data.Either (Either(..))
import Data.Bifunctor (lmap)

import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it, describeOnly, itOnly, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..)) as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)

import Grammar (Grammar, AST)
import Grammar.Parser (parser) as Grammar
import AST.Parser (parse) as WithGrammar

import Parsing (Parser, runParser, ParseError(..), Position(..)) as P
import StringParser (Parser, runParser, ParseError(..), PosString) as SP


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" do
    let
      grm = parsesGrammar
      grmfile = parsesGrammarFile
      grmfail = failsToParseGrammarWithError
      withgrm = parsesWithGivenGrammarAs
      withgrmfile = parsesWithGrammarFromFile
      mkerr = mkParseError
      mkserr = mkSParseError
    describe "parsing grammars (inline)" $ do
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

    {-
    describe "parsing with grammars" $ do

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
      it "parsing strings fails" $
        withgrm
          "foa"
          """main :- "foo"."""
          """< Expected 'foo', but found 'foa' :: <main> text @0 >"""
      it "parsing chars" $
        withgrm
          "f"
          "main :- 'f'."
          """( 0 <main> char 0-1 )"""
      it "parsing chars fails" $
        withgrm
          "g"
          "main :- 'f'."
          """< Expected 'f', but found 'g' :: <main> char @0 >"""
      it "parsing negated chars" $
        withgrm
          "o"
          "main :- ^'f'."
          """( 0 <main> not-char 0-1 )"""
      it "parsing negated chars fails" $
        withgrm
          "f"
          "main :- ^'f'."
          """< Expected not to find 'f', but found 'f' :: <main> not-char @0 >"""
      it "parsing char ranges" $
        withgrm
          "f"
          "main :- [c-g]."
          """( 0 <main> char-range 0-1 )"""
      it "parsing char ranges fails" $
        withgrm
          "a"
          "main :- [c-g]."
          """< Expected character in range from 'c' to 'g', but found 'a' :: <main> char-range @0 >"""
      it "parsing char sequences" $
        withgrm
          "foo"
          "main :- ['f','o','o']."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 char 0-1 ) : ( 0 seq:1 char 1-2 ) : ( 0 seq:2 char 2-3 ) )"
      it "parsing char sequences fails" $
        withgrm
          "fxo"
          "main :- ['f','o','o']."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 char 0-1 ) : < Expected 'o', but found 'x' :: seq:1 char @1 > : ( 0 seq:2 char 2-3 ) )"
      it "parsing any-char sequences" $
        withgrm
          "foo"
          "main :- [.,.,.]."
          "( 0 <main> seqnc 0-3 | ( 0 seq:0 any 0-1 ) : ( 0 seq:1 any 1-2 ) : ( 0 seq:2 any 2-3 ) )"
      it "parsing any-char sequences fails" $
        withgrm
          "fo"
          "main :- [.,.,.]."
          "( 0 <main> seqnc 0-2 | ( 0 seq:0 any 0-1 ) : ( 0 seq:1 any 1-2 ) : < Expected any character, but found end-of-input :: seq:2 any @2 > )"
      it "parsing char's and rule sequences" $
        withgrm "foo"
          """main :- [f,o,o].
          f :- 'f'.
          o :- 'o'.
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing char's and rule sequences fails" $
        withgrm "foo"
          """main :- [f,o,f].
          f :- 'f'.
          o :- 'o'.
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:o char 1-2 ) : < Expected 'f', but found 'o' :: rule:f char @2 > )" -- TODO: must be failed character rule inside `rule:f`
      it "parsing sequences with empty items" $
        withgrm "foo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:nothing text 1-1 ) : ( 0 rule:o char 1-2 ) : ( 0 rule:nothing text 2-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing sequences with empty items fails" $
        withgrm "fxo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          "( 0 <main> seqnc 0-3 | ( 0 rule:f char 0-1 ) : ( 0 rule:nothing text 1-1 ) : < Expected 'o', but found 'x' :: rule:o char @1 > : ( 0 rule:nothing text 2-2 ) : ( 0 rule:o char 2-3 ) )"
      it "parsing char choice" $
        withgrm
          "o"
          "main :- ('f'|'o'|'o')."
          "( 0 <main> choice 0-1 | ( 0 ch:1 char 0-1 ) )"
      it "parsing char choice fails" $
        withgrm
          "x"
          "main :- ('f'|'o'|'o')."
          "( 0 <main> choice 0-0 | < None of choices matched input :: <main> choice @0 | < Expected 'f', but found 'x' :: ch:0 char @0 > : < Expected 'o', but found 'x' :: ch:1 char @0 > : < Expected 'o', but found 'x' :: ch:2 char @0 > > )"
          -- FIXME:
          -- should better be either:
          --        "< None of choices matched input :: <main> choice @0 | < Expected 'f', but found 'x' :: ch:0 char @0 > : < Expected 'o', but found 'x' :: ch:1 char @0 > : < Expected 'o', but found 'x' :: ch:2 char @0 > >"
          -- ...or: "( 0 <main> choice 0-0 | < Expected 'f', but found 'x' :: rule:f char @0 > : < Expected 'o', but found 'x' :: rule:f char @0 > : < Expected 'o', but found 'x' :: rule:f char @0 > )"
          -- it seems the choice failure is a single child of `Node` in this case
      it "parsing rep/sep" $
          withgrm "f,o,o"
            """main :- repSep(fo,',').
            fo :- ('f'|'o').
            """
            "( 0 <main> repsep 0-5 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 sep char 1-2 ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) : ( 0 sep char 3-4 ) : ( 0 rule:fo choice 4-5 | ( 0 ch:1 char 4-5 ) ) : < Expected ',', but found end-of-input :: sep char @5 > )"
      it "parsing rep/sep fails when there's hanging separator" $
          withgrm "f,o,x"
            """main :- repSep(fo,',').
            fo :- ('f'|'o').
            """
            "( 0 <main> repsep 0-4 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 sep char 1-2 ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) : ( 0 sep char 3-4 ) : ( 0 rule:fo choice 4-4 | < None of choices matched input :: rule:fo choice @4 | < Expected 'f', but found 'x' :: ch:0 char @4 > : < Expected 'o', but found 'x' :: ch:1 char @4 > > ) : < Expected ',', but found 'x' :: sep char @4 > )"
      it "parsing rep/sep 2" $
        withgrm "foo"
          """main :- repSep(fo,"").
          fo :- ('f'|'o').
          """
          "( 0 <main> repsep 0-3 | ( 0 rule:fo choice 0-1 | ( 0 ch:0 char 0-1 ) ) : ( 0 sep text 1-1 ) : ( 0 rule:fo choice 1-2 | ( 0 ch:1 char 1-2 ) ) : ( 0 sep text 2-2 ) : ( 0 rule:fo choice 2-3 | ( 0 ch:1 char 2-3 ) ) : < Expected '', but found end-of-input :: sep text @3 > )"
      it "parsing rep/sep when empty" $
        withgrm ""
          """main :- repSep(" ","\n")."""
          "( 0 <main> repsep 0-0 | < Expected ' ', but found end-of-input :: rep text @0 > )"
      it "parsing rep/sep when empty 2" $
        withgrm "\n"
          """main :- repSep(" ","\n")."""
          "( 0 <main> repsep 0-0 | < Expected ' ', but found '\n' :: rep text @0 > )"
      -- pending' "parsing rep/sep when empty 3" $
      --  withgrm ""
      --    """main :- repSep("","")."""
      --    "( 0 <main> repsep 0-0 | ∅)"
      it "parsing rep/sep when empty 4" $
        withgrm ""
          """main :- repSep("a","b")."""
          "( 0 <main> repsep 0-0 | < Expected 'a', but found end-of-input :: rep text @0 > )"
      it "parsing rep/sep when empty 5" $
        withgrm ""
          """main :- repSep("a","")."""
          "( 0 <main> repsep 0-0 | < Expected 'a', but found end-of-input :: rep text @0 > )"
      it "parsing rep/sep when empty 6" $
        withgrm ""
          """main :- repSep('a',"")."""
          "( 0 <main> repsep 0-0 | < Expected 'a', but found end-of-input :: rep char @0 > )"
      it "parsing rep/sep with single element" $
        withgrm "2"
          """main :- repSep("2","")."""
          "( 0 <main> repsep 0-1 | ( 0 rep text 0-1 ) : < Expected '', but found end-of-input :: sep text @1 > )"
      it "parsing rep/sep with single element as char sequence" $
        withgrm "2"
          """main :- repSep([0-9],"")."""
          "( 0 <main> repsep 0-1 | ( 0 rep char-range 0-1 ) : < Expected '', but found end-of-input :: sep text @1 > )"
      it "capture works" $
        withgrm "[a-z]"
          """main :- ['[',from:char,'-',to:char,']'].
          char :- ('a'|'z').
          """
          "( 0 <main> seqnc 0-5 | ( 0 seq:0 char 0-1 ) : ( 0 rule:from choice 1-2 | ( 0 ch:0 char 1-2 ) ) : ( 0 seq:2 char 2-3 ) : ( 0 rule:to choice 3-4 | ( 0 ch:1 char 3-4 ) ) : ( 0 seq:4 char 4-5 ) )"
      it "plain text grammar works" $
        withgrm "foobar"
          """main :- repSep(., "")."""
          "( 0 <main> repsep 0-6 | ( 0 rep any 0-1 ) : ( 0 sep text 1-1 ) : ( 0 rep any 1-2 ) : ( 0 sep text 2-2 ) : ( 0 rep any 2-3 ) : ( 0 sep text 3-3 ) : ( 0 rep any 3-4 ) : ( 0 sep text 4-4 ) : ( 0 rep any 4-5 ) : ( 0 sep text 5-5 ) : ( 0 rep any 5-6 ) : < Expected '', but found end-of-input :: sep text @6 > )"
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
        withgrm "2"
          "main :- [a-z]."
          "< Expected character in range from 'a' to 'z', but found '2' :: <main> char-range @0 >"
      it "parsing char ranges in sequences" $
        withgrm "2"
          "main :- [[0-9]]."
          "( 0 <main> seqnc 0-1 | ( 0 seq:0 char-range 0-1 ) )"
      it "parsing char ranges in sequences 2" $
        withgrm "2"
          """main :- [[0-9],repSep(" ","")]."""
          "( 0 <main> seqnc 0-1 | ( 0 seq:0 char-range 0-1 ) : ( 0 seq:1 repsep 1-1 | < Expected ' ', but found end-of-input :: rep text @1 > ) )"
      it "parsing empty rep/seps" $
        withgrm "2"
          """main :- [[0-9],repSep([0-9],"")]."""
          "( 0 <main> seqnc 0-1 | ( 0 seq:0 char-range 0-1 ) : ( 0 seq:1 repsep 1-1 | < Expected character in range from '0' to '9', but found end-of-input :: rep char-range @1 > ) )"
      it "parsing empty rep/seps 2" $
        withgrm "2a"
          """main :- [[0-9],repSep(" ",""),'a']."""
          "( 0 <main> seqnc 0-2 | ( 0 seq:0 char-range 0-1 ) : ( 0 seq:1 repsep 1-1 | < Expected ' ', but found 'a' :: rep text @1 > ) : ( 0 seq:2 char 1-2 ) )"
      it "parsing empty rep/seps 3" $
        withgrm "t = 25"
          """main :- [[a-z], repSep([a-z], ""), " = 25"]."""
          "( 0 <main> seqnc 0-6 | ( 0 seq:0 char-range 0-1 ) : ( 0 seq:1 repsep 1-1 | < Expected character in range from 'a' to 'z', but found ' ' :: rep char-range @1 > ) : ( 0 seq:2 text 1-6 ) )"
      it "parsing choices" $
        withgrm "x"
          "main :- ('y' | 'x')."
          "( 0 <main> choice 0-1 | ( 0 ch:1 char 0-1 ) )"
      pending' "parsing choices 2" $ -- so that empty result is considered failure?
        withgrm "x"
          "main :- (\"\" | 'x')."
          "( 0 <main> choice 0-1 | ( 0 ch:1 char 0-1 ) )"
      it "parsing choices 3" $
        withgrm "x"
          "main :- ('x' | \"\")."
          "( 0 <main> choice 0-1 | ( 0 ch:0 char 0-1 ) )"
      it "parsing choices 4" $
        withgrm "x"
          "main :- (['x', '='] | 'x')."
          "( 0 <main> choice 0-1 | ( 0 ch:0 seqnc 0-1 | ( 0 seq:0 char 0-1 ) : < Expected '=', but found end-of-input :: seq:1 char @1 > ) )"
      it "parsing choices 5" $
        withgrm ""
          "main :- ('x' | \"\")."
          "( 0 <main> choice 0-0 | ( 0 ch:1 text 0-0 ) )"
      it "parsing choices 6" $
        withgrm "x"
          "main :- ([a-z] | \"\")."
          "( 0 <main> choice 0-1 | ( 0 ch:0 char-range 0-1 ) )"
      -}
      -- let
      --   identifierGrammar =
      --     """main :- ident.
      --        alpha :- ([a-z] | [A-Z] | "_").
      --        num :- [0-9].
      --        alphaNum :- (alpha | num).
      --        ident :- [alpha, repSep((alphaNum | "."), "")]."""

      let
        identifierGrammar =
          """main :- alphaNum.
             alpha :- ([a-z] | [A-Z] | "_").
             num :- [0-9].
             alphaNum :- (alpha | num).
             ident :- [alphaNum, repSep((alphaNum | "."), "")]."""

      {-

      it "parsing identifier rule" $
        withgrm "t"
          identifierGrammar
          "( 0 rule:ident seqnc 0-1 | ( 0 rule:alpha choice 0-1 | ( 0 ch:0 char-range 0-1 ) ) : ( 0 seq:1 repsep 1-1 | < None of choices matched input :: rep choice @1 > ) )"
          --"( 0 rule:ident seqnc 0-1 | ( 0 rule:alpha choice 0-1 | ( 0 ch:0 char-range 0-1 ) ) : ( 0 seq:1 repsep 1-1 | ∅ ) )"
      -}

      it "parsing identifier rule 2" $
        withgrm "0"
          identifierGrammar
          "( 0 rule:ident seqnc 0-2 | ( 0 rule:alpha choice 0-1 | ( 0 ch:0 char-range 0-1 ) ) : ( 0 seq:1 repsep 1-2 | ( 0 rep choice 1-2 | ( 0 rule:alphaNum choice 1-2 | ( 0 rule:num char-range 1-2 ) ) ) ) )"

  {-
      it "parsing identifier rule 3" $
        withgrm "t0 "
          identifierGrammar
          "( 0 rule:ident seqnc 0-2 | ( 0 rule:alpha choice 0-1 | ( 0 ch:0 char-range 0-1 ) ) : ( 0 seq:1 repsep 1-2 | ( 0 rep choice 1-2 | ( 0 rule:alphaNum choice 1-2 | ( 0 rule:num char-range 1-2 ) ) ) ) )"
      -}

{-
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
-}



parsesGrammar ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String → m Unit
parsesGrammar grammarStr expectation =
  (show <$> P.runParser grammarStr Grammar.parser) `shouldEqual` (Right expectation)


parsesGrammarFile ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m ⇒ String → m Unit
parsesGrammarFile fileName = do
  grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
  expectation <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar.expected"
  let eGrammar =  P.runParser grammarStr Grammar.parser
  liftEffect $ writeTextFile Encoding.UTF8 ("./test/grammars/" <> fileName <> ".grammar.result") $ reportE eGrammar
  (show <$> eGrammar) `shouldEqual` (Right expectation)


failsToParseGrammarWithError ∷ ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → P.ParseError → m Unit
failsToParseGrammarWithError grammarStr error =
  (show <$> P.runParser grammarStr Grammar.parser) `shouldEqual` (Left error)


mkParseError :: String -> Int -> Int -> Int -> P.ParseError
mkParseError err line column index = P.ParseError err $ P.Position { line, column, index }


mkSParseError :: String -> Int -> SP.ParseError
mkSParseError error pos = { error, pos }


parsesWithGivenGrammarAs :: ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ String → String -> String → m Unit
parsesWithGivenGrammarAs str grammarStr expectation =
  let
    eGrammar = P.runParser grammarStr Grammar.parser
    buildAst grammar = WithGrammar.parse grammar (const 0) str
  in (show <$> buildAst <$> lmap convertError eGrammar) `shouldEqual` (Right expectation)


parsesWithGrammarFromFile :: ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Error m => String -> m Unit
parsesWithGrammarFromFile fileName = do
    grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
    sourceStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src"
    expectationStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src.expected"
    let
      eGrammar = P.runParser grammarStr Grammar.parser
      buildAst :: Grammar -> AST Int
      buildAst grammar = WithGrammar.parse grammar (const 0) sourceStr
      eAST = buildAst <$> lmap convertError eGrammar
    liftEffect $ writeTextFile Encoding.UTF8 ("./test/sources/" <> fileName <> ".src.result") $ reportE eAST
    (show <$> eAST) `shouldEqual` (Right expectationStr)


convertError :: P.ParseError -> SP.ParseError
convertError (P.ParseError error (P.Position { index })) = { error, pos : index }


reportE :: forall a err. Show err => Show a => Either err a -> String
reportE = case _ of
    Left error -> "error::" <> show error
    Right grammar -> show grammar