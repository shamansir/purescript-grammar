module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error) as Ex

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Bifunctor (lmap)

import Effect.Aff (launchAff_)
import Test.Spec (describe, it, pending', itOnly)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..)) as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)

import Yoga.Tree (showTree)
import Yoga.Tree.Extended (Tree(..))
import Yoga.Tree.Extended (node, leaf) as Tree

import Grammar (Grammar, Rule(..), WhichChar(..), CharX(..))
import Grammar (toTree) as Grammar
import Grammar.Parser (parser) as Grammar
import Grammar.Self.Parser (grammar) as Self
import Grammar.Self.Extract (extract) as Self
import Grammar.AST (AST(..), ASTNode, Attempt(..), Expected(..), Found(..), Error(..), ruleOf, fillChunks)
import Grammar.AST.Location (Range)
import Grammar.AST.Chunk (content) as Chunk
import Grammar.AST.LocatedChunk (posFinderFor) as LocChunk
import Grammar.AST.Parser (parse, parseBy) as WithGrammar

import Parsing (runParser, ParseError(..), Position(..)) as P
import StringParser (ParseError) as SP


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

    describe "parsing with grammars" $ do

      it "failing to parse" $
        withgrm
          "?"
          """main :- "foo"."""
          $ l_text_err (Expected "foo") (Found "?") { pos : 0 }
      it "parsing strings" $
        withgrm
          "foo"
          """main :- "foo"."""
          $ l_text (Expected "foo") { start : 0, end : 3 }
      it "parsing strings at end-of-input" $
        withgrm
          ""
          """main :- "foo"."""
          $ l_text_err (Expected "foo") EOI { pos : 0 }
      it "parsing strings fails 2" $
        withgrm
          "foa"
          """main :- "foo"."""
          $ l_text_err (Expected "foo") (Found "foa") { pos : 0 }
      it "parsing chars" $
        withgrm
          "f"
          "main :- 'f'."
          $ l_char (Expected 'f') { at : 0 }
      it "parsing chars fails" $
        withgrm
          "g"
          "main :- 'f'."
          $ l_char_err (Expected 'f') (Found 'g') { pos : 0 }
      it "parsing negated chars" $
        withgrm
          "o"
          "main :- ^'f'."
          $ l_neg_char (Expected 'f') { at : 0 }
      it "parsing negated chars fails" $
        withgrm
          "f"
          "main :- ^'f'."
          $ l_neg_char_err (Expected 'f') { pos : 0 }
      it "parsing char ranges" $
        withgrm
          "f"
          "main :- [c-g]."
          $ l_char_rng { from : 'c', to : 'g' } { at : 0 } -- FIXME: passed rule info is not shown so with any from/to this test passes
      it "parsing char ranges fails" $
        withgrm
          "a"
          "main :- [c-g]."
          $ l_char_rng_err { from : 'c', to : 'g' } (Found 'a') { pos : 0 }
      it "parsing char sequences" $
        withgrm
          "foo"
          "main :- ['f','o','o']."
          $ n_seq { start : 0, end : 3 }
            [ l_char (Expected 'f') { at : 0 }
            , l_char (Expected 'o') { at : 1 }
            , l_char (Expected 'o') { at : 2 }
            ]
      it "parsing char sequences fails" $
        withgrm
          "fxo"
          "main :- ['f','o','o']."
          $ n_seq_err { pos : 1, entry : 1 }
            [ l_char (Expected 'f') { at : 0 }
            , l_char_err (Expected 'o') (Found 'x') { pos : 1 }
            ]
      it "parsing any-char sequences" $
        withgrm
          "foo"
          "main :- [.,.,.]."
          $ n_seq { start : 0, end : 3 }
            [ l_char_any { at : 0 }
            , l_char_any { at : 1 }
            , l_char_any { at : 2 }
            ]
      it "parsing any-char sequences fails" $
        withgrm
          "fo"
          "main :- [.,.,.]."
          $ n_seq_err { pos : 2, entry : 2 }
            [ l_char_any { at : 0 }
            , l_char_any { at : 1 }
            , l_char_any_err EOI { pos : 2 }
            ]
      it "parsing char's and rule sequences" $
        withgrm "foo"
          """main :- [f,o,o].
          f :- 'f'.
          o :- 'o'.
          """
          $ n_seq { start : 0, end : 3 }
            [ n_ref "f" { start : 0, end : 1 } $ l_char (Expected 'f') { at : 0 }
            , n_ref "o" { start : 1, end : 2 } $ l_char (Expected 'o') { at : 1 }
            , n_ref "o" { start : 2, end : 3 } $ l_char (Expected 'o') { at : 2 }
            ]
      it "parsing char's and rule sequences fails" $
        withgrm "foo"
          """main :- [f,o,f].
          f :- 'f'.
          o :- 'o'.
          """
          $ n_seq_err { pos : 2, entry : 2 }
            [ n_ref "f" { start : 0, end : 1 } $ l_char (Expected 'f') { at : 0 }
            , n_ref "o" { start : 1, end : 2 } $ l_char (Expected 'o') { at : 1 }
            , n_ref_err "f" { pos : 2 } $ l_char_err (Expected 'f') (Found 'o') { pos : 2 }
            ]
      it "parsing sequences with empty items" $
        withgrm "foo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          $ n_seq { start : 0, end : 3 }
            [ n_ref "f" { start : 0, end : 1 } $ l_char (Expected 'f') { at : 0 }
            , n_ref "nothing" { start : 1, end : 1 } $ l_text (Expected "") { start : 1, end : 1 }
            , n_ref "o" { start : 1, end : 2 } $ l_char (Expected 'o') { at : 1 }
            , n_ref "nothing" { start : 2, end : 2 } $ l_text (Expected "") { start : 2, end : 2 }
            , n_ref "o" { start : 2, end : 3 } $ l_char (Expected 'o') { at : 2 }
            ]
      it "parsing sequences with empty items fails" $
        withgrm "fxo"
          """main :- [f,nothing,o,nothing,o].
          f :- 'f'.
          o :- 'o'.
          nothing :- "".
          """
          $ n_seq_err { pos : 1, entry : 2 }
            [ n_ref "f" { start : 0, end : 1 } $ l_char (Expected 'f') { at : 0 }
            , n_ref "nothing" { start : 1, end : 1 } $ l_text (Expected "") { start : 1, end : 1 }
            , n_ref_err "o" { pos : 1 } $ l_char_err (Expected 'o') (Found 'x') { pos : 1 }
            ]
      it "parsing char choice" $
        withgrm
          "o"
          "main :- ('f'|'o'|'o')."
          $ n_choice { start : 0, end : 1 }
            [ l_char_err (Expected 'f') (Found 'o') { pos : 0 }
            , l_char (Expected 'o') { at : 0 }
            ]
      it "parsing char choice fails" $
        withgrm
          "x"
          "main :- ('f'|'o'|'o')."
          $ n_choice_err { pos : 0 }
            [ l_char_err (Expected 'f') (Found 'x') { pos : 0 }
            , l_char_err (Expected 'o') (Found 'x') { pos : 0 }
            , l_char_err (Expected 'o') (Found 'x') { pos : 0 }
            ]
      it "parsing rep/sep" $
          withgrm "f,o,o"
            """main :- repSep(fo,',').
            fo :- ('f'|'o').
            """
            $ n_rep_sep { start : 0, end : 5 }
              [ n_ref "fo" { start : 0, end : 1 }
                  $ n_choice { start : 0, end : 1 }
                      [ l_char (Expected 'f') { at : 0 } ]
              , l_char (Expected ',') { at : 1 }
              , n_ref "fo" { start : 2, end : 3 }
                  $ n_choice { start : 2, end : 3 }
                      [ l_char_err (Expected 'f') (Found 'o') { pos : 2 }
                      , l_char (Expected 'f') { at : 2 }
                      ]
              , l_char (Expected ',') { at : 3 }
              , n_ref "fo" { start : 4, end : 5 }
                  $ n_choice { start : 4, end : 5 }
                      [ l_char_err (Expected 'f') (Found 'o') { pos : 4 }
                      , l_char (Expected 'o') { at : 4 }
                      ]
              , l_char_err (Expected ',') EOI { pos : 5 }
              ]
      it "parsing rep/sep fails when there's hanging separator in input (should pass)" $ -- or else we could use sequence?
          withgrm "f,o,x"
            """main :- repSep(fo,',').
            fo :- ('f'|'o').
            """
            -- $ n_rep_sep_err { pos : 4, entry : 3 } -- TODO: or should it fail?
            $ n_rep_sep { start : 0, end : 4 }
              [ n_ref "fo" { start : 0, end : 1 }
                  $ n_choice { start : 0, end : 1 }
                      [ l_char (Expected 'f') { at : 0 } ]
              , l_char (Expected ',') { at : 1 }
              , n_ref "fo" { start : 2, end : 3 }
                  $ n_choice { start : 2, end : 3 }
                      [ l_char_err (Expected 'f') (Found 'o') { pos : 2 }
                      , l_char (Expected 'f') { at : 2 }
                      ]
              , l_char (Expected ',') { at : 3 }
              , n_ref_err "fo" { pos : 4 }
                  $ n_choice_err { pos : 4 }
                      [ l_char_err (Expected 'f') (Found 'x') { pos : 4 }
                      , l_char_err (Expected 'o') (Found 'x') { pos : 4 }
                      ]
              ]
      it "parsing rep/sep with empty separators (should pass)" $
        withgrm "foo"
          """main :- repSep(fo,"").
          fo :- ('f'|'o').
          """
          $ n_rep_sep { start : 0, end : 3 }
              [ n_ref "fo" { start : 0, end : 1 } $ n_choice { start : 0, end : 1 } [ l_char (Expected 'f') { at : 0 } ]
              , l_text (Expected "") { start : 1, end : 1 }
              , n_ref "fo" { start : 1, end : 2 } $ n_choice { start : 1, end : 2 } [ l_char_err (Expected 'f') (Found 'o') { pos : 1 }, l_char (Expected 'f') { at : 1 } ]
              , l_text (Expected "") { start : 2, end : 2 }
              , n_ref "fo" { start : 2, end : 3 } $ n_choice { start : 2, end : 3 } [ l_char_err (Expected 'f') (Found 'o') { pos : 2 }, l_char (Expected 'o') { at : 2 } ]
              , l_text_eoi_err { pos : 3 }
              ]
      it "parsing rep/sep with empty separators and repeating input (should compute the complete input)" $
        withgrm "aaa"
          """main :- repSep('a',"").
          """
          $ n_rep_sep { start : 0, end : 3 }
              [ l_char (Expected 'a') { at : 0 }
              , l_text (Expected "") { start : 1, end : 1 }
              , l_char (Expected 'a') { at : 1 }
              , l_text (Expected "") { start : 2, end : 2 }
              , l_char (Expected 'a') { at : 2 }
              , l_text_eoi_err { pos : 3 }
              ]
      it "parsing rep/sep when input is empty (should pass)" $
        withgrm ""
          """main :- repSep(" ","\n")."""
          -- $ n_rep_sep_err { pos : 0, entry : 0 } -- TODO: or should it fail?
          $ n_rep_sep { start : 0, end : 0 }
            [ l_text_err (Expected " ") EOI { pos : 0 }
            ]
      it "parsing rep/sep when input starts with a separator (should pass)" $
        withgrm "\n"
          """main :- repSep(" ","\n")."""
          -- $ n_rep_sep_err { pos : 0, entry : 0 } -- TODO: or should it fail?
          $ n_rep_sep { start : 0, end : 0 }
            [ l_text_err (Expected " ") (Found "\n") { pos : 0 }
            ]
      it "parsing rep/sep when input is empty, but also rep & sep are both empty as well  (should pass)" $
        withgrm ""
          """main :- repSep("","")."""
          $ n_rep_sep { start : 0, end : 0 }
            [ l_text_eoi_err { pos : 0 }
            ]
      it "parsing rep/sep when input is empty, but both rep & sep are non-empty (should pass)" $
        withgrm ""
          """main :- repSep("a","b")."""
          $ n_rep_sep { start : 0, end : 0 }
            [ l_text_err (Expected "a") EOI { pos : 0 }
            ]
      it "parsing rep/sep when input is empty, sep is empty, but rep is non-enpty (should pass)" $
        withgrm ""
          """main :- repSep("a","")."""
          $ n_rep_sep { start : 0, end : 0 }
            [ l_text_err (Expected "a") EOI { pos : 0 }
            ]
      it "parsing rep/sep when input is empty, sep is empty, ane rep is char rule (should pass)" $
        withgrm ""
          """main :- repSep('a',"")."""
          $ n_rep_sep { start : 0, end : 0 }
            [ l_char_err (Expected 'a') EOI { pos : 0 }
            ]
      it "parsing rep/sep with single rep element (should pass)" $
        withgrm "2"
          """main :- repSep("2","")."""
          $ n_rep_sep { start : 0, end : 1 }
            [ l_text (Expected "2") { start : 0, end : 1 }
            , l_text_eoi_err { pos : 1 }
            ]
      it "parsing rep/sep with single element as char sequence (should pass)" $
        withgrm "2"
          """main :- repSep([0-9],"")."""
          $ n_rep_sep { start : 0, end : 1 }
            [ l_char_rng { from : '0', to : '9' } { at : 0 }
            , l_text_eoi_err { pos : 1 }
            ]
      it "capture works" $
        withgrm "[a-z]"
          """main :- ['[',from:char,'-',to:char,']'].
          char :- ('a'|'z').
          """
          $ n_seq { start : 0, end : 5 }
            [ l_char (Expected '[') { at : 0 }
            , n_ref_capt { name : "char", as : "from" } { start : 1, end : 2 }
                $ n_choice { start : 1, end : 2 }
                  [ l_char (Expected 'a') { at : 1 }
                  ] -- FIXME: even when passed, choice should store failed tries so that they match by index
            , l_char (Expected '-') { at : 2 }
            , n_ref_capt { name : "char", as : "to" } { start : 3, end : 4 }
                $ n_choice { start : 3, end : 4 }
                  [ l_char_err (Expected 'a') (Found 'z') { pos : 3 }
                  , l_char (Expected 'z') { at : 3 }
                  ] -- TODO: change all the previous `n_choice` tests, so even when choice passed, it should store failed tries so that they match by index (or else we don't know which instance of rule has passed), or may be store this info in the match
            , l_char (Expected ']') { at : 4 }
            ]
      it "plain text grammar works" $
        withgrm "foobar"
          """main :- repSep(., "")."""
          $ n_rep_sep { start : 0, end : 6 }
              [ l_char_any { at : 0 }
              , l_text (Expected "") { start : 1, end : 1 }
              , l_char_any { at : 1 }
              , l_text (Expected "") { start : 2, end : 2 }
              , l_char_any { at : 2 }
              , l_text (Expected "") { start : 3, end : 3 }
              , l_char_any { at : 3 }
              , l_text (Expected "") { start : 4, end : 4 }
              , l_char_any { at : 4 }
              , l_text (Expected "") { start : 5, end : 5 }
              , l_char_any { at : 5 }
              , l_text_eoi_err { pos : 6 }
              ]
      it "parsing newlines (as chars)" $
        withgrm "\n"
          """main :- '\n'."""
          $ l_char (Expected '\n') { at : 0 }
      it "parsing newlines (as strings)" $
        withgrm "\n"
          """main :- "\n"."""
          $ l_text (Expected "\n") { start : 0, end : 1 }
      it "parsing char ranges" $
        withgrm "234"
          "main :- [digit,digit,digit].\ndigit :- [0-9]."
          $ n_seq { start : 0, end : 3 }
            [ n_ref "digit" { start : 0, end : 1 } $ l_char_rng { from : '0', to : '9' } { at : 0 }
            , n_ref "digit" { start : 1, end : 2 } $ l_char_rng { from : '0', to : '9' } { at : 1 }
            , n_ref "digit" { start : 2, end : 3 } $ l_char_rng { from : '0', to : '9' } { at : 2 }
            ]
      it "num is not parsed as alpha" $
        withgrm "2"
          "main :- [a-z]."
          $ l_char_rng_err { from : 'a', to : 'z' } (Found '2') { pos : 0 }
      it "parsing char ranges in sequences" $
        withgrm "2"
          "main :- [[0-9]]."
          $ n_seq { start : 0, end : 1 } [ l_char_rng { from : '0', to : '9' } { at : 0 } ]
      it "parsing char ranges in sequences 2" $
        withgrm "2"
          """main :- [[0-9],repSep(" ","")]."""
          $ n_seq { start : 0, end : 1 }
              [ l_char_rng { from : '0', to : '9' } { at : 0 }
              , n_rep_sep { start : 1, end : 1 }
                  [ l_text_err (Expected " ") EOI { pos : 1 }
                  ]
              ]
      it "parsing empty rep/seps with char-ranges" $
        withgrm "2"
          """main :- [[0-9],repSep([0-9],"")]."""
          $ n_seq { start : 0, end : 1 }
              [ l_char_rng { from : '0', to : '9' } { at : 0 }
              , n_rep_sep { start : 1, end : 1 }
                  [ l_char_rng_err { from : '0', to : '9' } EOI { pos : 1 }
                  ]
              ]
      it "parsing empty rep/seps between matches" $
        withgrm "2a"
          """main :- [[0-9],repSep(" ",""),'a']."""
          $ n_seq { start : 0, end : 2 }
              [ l_char_rng { from : '0', to : '9' } { at : 0 }
              , n_rep_sep { start : 1, end : 1 }
                  [ l_text_err (Expected " ") (Found "a") { pos : 1 }
                  ]
              , l_char (Expected 'a') { at : 1 }
              ]
      it "parsing empty rep/seps between matches 2" $
        withgrm "t = 25"
          """main :- [[a-z], repSep([a-z], ""), " = 25"]."""
          $ n_seq { start : 0, end : 6 }
              [ l_char_rng { from : 'a', to : 'z' } { at : 0 }
              , n_rep_sep { start : 1, end : 1 }
                  [ l_char_rng_err { from : 'a', to : 'z' } (Found ' ') { pos : 1 }
                  ]
              , l_text (Expected " = 25") { start : 1, end : 6 }
              ]
      it "parsing generic choices" $
        withgrm "x"
          "main :- ('y' | 'x')."
          $ n_choice { start : 0, end : 1 }
              [ l_char_err (Expected 'y') (Found 'x') { pos : 0 }
              , l_char (Expected 'x') { at : 0 }
              ]
      it "parsing choices with the first empty option (the first empty option should pass)" $ -- even though the second one seems to fit
        withgrm "x"
          "main :- (\"\" | 'x')."
          $ n_choice { start : 0, end : 0 }
            [ l_text (Expected "") { start : 0, end : 0 }
            ]
      it "parsing choices with the last empty option (the first fitting option should pass)" $
        withgrm "x"
          "main :- ('x' | \"\")."
          $ n_choice { start : 0, end : 1 }
            [ l_char (Expected 'x') { at : 0 }
            ]
      it "parsing choices with  the last empty option continued" $
        withgrm "x"
          "main :- ('y' | 'x' | \"\")."
          $ n_choice { start : 0, end : 1 }
            [ l_char_err (Expected 'y') (Found 'x') { pos : 0 }
            , l_char (Expected 'x') { at : 0 }
            ]
      it "parsing choices with overlapping sequences" $
        withgrm "x"
          "main :- (['x', '='] | 'x')."
          $ n_choice { start : 0, end : 1 }
            [ n_seq_err { entry : 1, pos : 1 }
              [ l_char (Expected 'x') { at : 0 }
              , l_char_err (Expected '=') EOI { pos : 1 }
              ]
            , l_char (Expected 'x') { at : 0 }
            ]
      it "parsing choices with empty option on empty input" $
        withgrm ""
          "main :- ('x' | \"\")."
          $ n_choice { start : 0, end : 0 }
            [ l_char_err (Expected 'x') EOI { pos : 0 }
            , l_text (Expected "") { start : 0, end : 0 }
            ]
      it "parsing choices with empty option on a meaningful input" $
        withgrm "x"
          "main :- ([a-z] | \"\")."
          $ n_choice { start : 0, end : 1 }
            [ l_char_rng { from : 'a', to : 'z' } { at : 0 }
            ]

      let
        identifierGrammar =
          """main :- ident.
             alpha :- ([a-z] | [A-Z] | "_").
             num :- [0-9].
             alphaNum :- (alpha | num).
             ident :- [alpha, repSep((alphaNum | "."), "")]."""


      it "parsing identifier rule" $
        withgrm "t"
          identifierGrammar
            $ n_ref "ident" { start : 0, end : 1 }
            $ n_seq { start : 0, end : 1 }
                [ n_ref "alpha" { start : 0, end : 1 }
                    $ n_choice { start : 0, end : 1 }
                    [ l_char_rng { from : 'a', to : 'z' } { at : 0 }
                    ]
                , n_rep_sep { start : 1, end : 1 }
                    [ n_choice_err { pos : 1 }
                        [ n_ref_err "alphaNum" { pos : 1 }
                            $ n_choice_err { pos : 1 }
                                [ n_ref_err "alpha" { pos : 1 }
                                    $ n_choice_err { pos : 1 }
                                    [ l_char_rng_err
                                        { from : 'a', to : 'z' } EOI { pos : 1 }
                                    , l_char_rng_err
                                        { from : 'A', to : 'Z' } EOI { pos : 1 }
                                    , l_text_err
                                        (Expected "_") EOI { pos : 1 }
                                    ]
                                , n_ref_err "num" { pos : 1 }
                                    $ l_char_rng_err
                                        { from : '0', to : '9' } EOI { pos : 1 }
                                ]
                        , l_text_err (Expected ".") EOI { pos : 1 }
                        ]
                    ]
                ]
      it "parsing identifier rule 2" $
        withgrm "2"
          identifierGrammar
            $ n_ref_err "ident" { pos : 0 }
            $ n_seq_err { pos : 0, entry : 0 }
                [ n_ref_err "alpha" { pos : 0 }
                    $ n_choice_err { pos : 0 }
                    [ l_char_rng_err { from : 'a', to : 'z' } (Found '2') { pos : 0 }
                    , l_char_rng_err
                        { from : 'A', to : 'Z' } (Found '2') { pos : 0 }
                    , l_text_err
                        (Expected "_") (Found "2") { pos : 0 }
                    ]
                ]

      it "parsing identifier rule 3" $
        withgrm "t0 "
          identifierGrammar
            $ n_ref "ident" { start : 0, end : 2 }
            $ n_seq { start : 0, end : 2 }
                [ n_ref "alpha" { start : 0, end : 1 }
                    $ n_choice { start : 0, end : 1 }
                    [ l_char_rng { from : 'a', to : 'z' } { at : 0 }
                    ]
                , n_rep_sep { start : 1, end : 2 }
                    [ n_choice { start : 1, end : 2 }
                        [ n_ref "alphaNum" { start : 1, end : 2 }
                            $ n_choice { start : 1, end : 2 }
                                [ n_ref_err "alpha" { pos : 1 }
                                    $ n_choice_err { pos : 1 }
                                    [ l_char_rng_err
                                        { from : 'a', to : 'z' } (Found '0') { pos : 1 }
                                    , l_char_rng_err
                                        { from : 'A', to : 'Z' } (Found '0') { pos : 1 }
                                    , l_text_err
                                        (Expected "_") (Found "0") { pos : 1 }
                                    ]
                                , n_ref "num" { start : 1, end : 2 }
                                    $ l_char_rng
                                        { from : '0', to : '9' } { at : 1 }
                                ]
                        ]
                    , l_text (Expected "") { start : 2, end : 2 }
                    , n_choice_err { pos : 2 }
                        [ n_ref_err "alphaNum" { pos : 2 }
                            $ n_choice_err { pos : 2 }
                                [ n_ref_err "alpha" { pos : 2 }
                                    $ n_choice_err { pos : 2 }
                                    [ l_char_rng_err
                                        { from : 'a', to : 'z' } (Found ' ') { pos : 2 }
                                    , l_char_rng_err
                                        { from : 'A', to : 'Z' } (Found ' ') { pos : 2 }
                                    , l_text_err
                                        (Expected "_") (Found " ") { pos : 2 }
                                    ]
                                , n_ref_err "num" { pos : 2 }
                                    $ l_char_rng_err
                                        { from : '0', to : '9' } (Found ' ') { pos : 2 }
                                ]
                        , l_text_err (Expected ".") (Found " ") { pos : 2 }
                        ]
                    ]
                  ]


    describe "grammars from files" $ do
        pending' "parses `blocks`" $
          withgrmfile "blocks"
        it "parses `contracts`" $
          withgrmfile "contracts"
        it "parses `datalog`" $
          withgrmfile "datalog"
        it "parses `datalog2`" $
          withgrmfile "datalog2"
        it "parses `fp`" $
          withgrmfile "fp"
        pending' "parses `grammar`" $
          withgrmfile "grammar"
        it "parses `grammar2`" $
          withgrmfile "grammar2"
        it "parses `json`" $
          withgrmfile "json"
        pending' "parses `modelica`" $
          withgrmfile "modelica"
        pending' "parses `opt`" $
          withgrmfile "opt"
        it "parses `plainText`" $
          withgrmfile "plainText"
        it "parses `sql`" $
          withgrmfile "sql"
        -- it "parses `test`" $
        --   withgrmfile "test"
        it "parses `treeSql`" $
          withgrmfile "treeSql"

    describe "converting to tree" $
      it "should properly convert" $
        convertsGrammarToTree "main :- 'a'." "<root>\n|----> <main>\n       |----> <ch:a>\n"

    describe "extract" $ do
      describe "should properly parse grammar and extract it from AST" $ do
        it "parses simple parser with strings" $ do
          parsesGivenGrammarWithGrammar "main :- \"foo\"." "main :- \"foo\".\n"
        it "parses simple parser with characters" $ do
          parsesGivenGrammarWithGrammar "main :- 'f'." "main :- 'f'.\n"
        it "parses simple parser with escaped characters" $ do
          parsesGivenGrammarWithGrammar
              """main :- ['"','\n','\\',"\n","\t",'\\']."""
              """main :- ['"','\n','\\',"\n","\t",'\\'].
              """
        it "parses simple parser with escaped characters 2" $ do
          parsesGivenGrammarWithGrammar """main :- ('"'|'\n'|"\n"|"\t"|"\\"|'\\').""" "main :- \"foo\".\n"
        it "parses simple parser with any char" $ do
          parsesGivenGrammarWithGrammar "main :- .." "main :- ..\n"
        it "parses simple parser with char range" $ do
          parsesGivenGrammarWithGrammar "main :- [a-z]." "main :- [a-z].\n"
        it "parses simple parser with not-characters" $ do
          parsesGivenGrammarWithGrammar "main :- [^'f']." "main :- [^'f'].\n"
        it "parses simple parser with not-characters & range" $ do
          parsesGivenGrammarWithGrammar "main :- [^[a-z]]." "main :- [^[a-z]].\n"
        it "parses simple parser with not-characters & any char" $ do
          parsesGivenGrammarWithGrammar "main :- [^.]." "main :- [^.].\n"
        it "parses simple parser with sequence" $ do
          parsesGivenGrammarWithGrammar "main :- ['a','b','c']." "main :- ['a','b','c'].\n"
        it "parses simple parser with choice" $ do
          parsesGivenGrammarWithGrammar "main :- ('a'|'b'|'c')." "main :- ('a'|'b'|'c').\n"
        it "parses simple parser with choice & sequence" $ do
          parsesGivenGrammarWithGrammar "main :- ('a'|'b'|['f','o','o']|'c')." "main :- ('a'|'b'|['f','o','o']|'c').\n"
        it "parses simple parser with sequence & choice" $ do
          parsesGivenGrammarWithGrammar "main :- ['a','b',('f'|'o'|'o'),'c']." "main :- ['a','b',('f'|'o'|'o'),'c'].\n"
        it "parses simple parser with repSep" $ do
          parsesGivenGrammarWithGrammar "main :- repSep(\"foo\",'a')." "main :- repSep(\"foo\",'a').\n"
        it "parses simple parser with repSep 2" $ do
          parsesGivenGrammarWithGrammar "main :- repSep(.,.)." "main :- repSep(.,.).\n"
        it "parses simple parser with ref to other rule" $ do
          parsesGivenGrammarWithGrammar "main :- foo.\nfoo :- \"foo\"." "main :- foo.\nfoo :- \"foo\"."
        it "parses simple parser with capture-ref to other rule" $ do
          parsesGivenGrammarWithGrammar "main :- as:foo.\nfoo :- \"foo\"." "main :- as:foo.\nfoo :- \"foo\"."

      describe "grammars from files" $ do
        it "parses `blocks`" $ do
          parsesGrammarWithGrammar "blocks"
        it "parses `contracts`" $ do
          parsesGrammarWithGrammar "contracts"
        it "parses `datalog`" $ do
          parsesGrammarWithGrammar "datalog"
        it "parses `datalog2`" $ do
          parsesGrammarWithGrammar "datalog2"
        it "parses `fp`" $ do
          parsesGrammarWithGrammar "fp"
        it "parses `grammar`" $ do
          parsesGrammarWithGrammar "grammar"
        it "parses `grammar2`" $ do
          parsesGrammarWithGrammar "grammar2"
        it "parses `json`" $ do
          parsesGrammarWithGrammar "json"
        it "parses `modelica`" $ do
          parsesGrammarWithGrammar "modelica"
        it "parses `opt`" $ do
          parsesGrammarWithGrammar "opt"
        it "parses `plainText`" $ do
          parsesGrammarWithGrammar "plainText"
        it "parses `sql`" $ do
          parsesGrammarWithGrammar "sql"
        it "parses `treeSql`" $ do
          parsesGrammarWithGrammar "treeSql"


    describe "chunks" $ do
      it "finding chunks" $ do
        let finder = LocChunk.posFinderFor "aaa\nbbbb\ncc"
        {-
              C0     C1     C2     C3     C4
        L0 : 00 a | 01 a | 02 a | 03 \n
        L1 : 04 b | 05 b | 06 b | 07 b | 08 \n
        L2 : 09 c | 10 c
        -}
        finder 0 `shouldEqual` { line : 0, column : 0 }
        finder 1 `shouldEqual` { line : 0, column : 1 }
        finder 3 `shouldEqual` { line : 0, column : 3 }
        finder 4 `shouldEqual` { line : 1, column : 0 }
        finder 6 `shouldEqual` { line : 1, column : 2 }
        finder 7 `shouldEqual` { line : 1, column : 3 }
        finder 10 `shouldEqual` { line : 2, column : 1 }
        finder 15 `shouldEqual` { line : 3, column : 0 } -- should be the last symbol?
        finder 20 `shouldEqual` { line : 3, column : 0 } -- should be the last symbol?


l_char :: Expected Char -> { at :: Int } -> ASTNode Int
l_char (Expected ch) { at } = Tree.leaf { rule : Char $ Single $ Raw ch, result : Match { start : at, end : at + 1 } 0 }


l_neg_char :: Expected Char -> { at :: Int } -> ASTNode Int
l_neg_char (Expected ch) { at } = Tree.leaf { rule : Char $ Not $ Raw ch, result : Match { start : at, end : at + 1 } 0 }


l_char_rng :: { from :: Char, to :: Char } -> { at :: Int } -> ASTNode Int
l_char_rng { from, to } { at } = Tree.leaf { rule : Char $ Range from to, result : Match { start : at, end : at + 1 } 0 }


l_char_any :: { at :: Int } -> ASTNode Int
l_char_any { at } = Tree.leaf { rule : Char Any, result : Match { start : at, end : at + 1 } 0 }


l_text :: Expected String -> Range -> ASTNode Int
l_text (Expected text) range = Tree.leaf { rule : Text text, result : Match range 0 }


l_char_err :: Expected Char -> Found Char -> { pos :: Int } -> ASTNode Int
l_char_err (Expected ch) found { pos } =
  Tree.leaf { rule : Char $ Single $ Raw ch, result : Fail pos $ CharacterError { expected : Expected $ Raw ch, found : found }  }


l_neg_char_err :: Expected Char -> { pos :: Int } -> ASTNode Int
l_neg_char_err (Expected ch) { pos } =
  Tree.leaf { rule : Char $ Not $ Raw ch, result : Fail pos $ NegCharacterError { notExpected : Expected $ Raw ch, found : Found ch }  }


l_char_rng_err :: { from :: Char, to :: Char } -> Found Char -> { pos :: Int } -> ASTNode Int
l_char_rng_err { from, to } found { pos } = Tree.leaf { rule : Char $ Range from to, result : Fail pos $ CharacterRangeError { found, from : Expected from, to : Expected to } }


l_char_any_err :: Found Char -> { pos :: Int } -> ASTNode Int
l_char_any_err found { pos } = Tree.leaf { rule : Char Any, result : Fail pos $ AnyCharacterError { found } }


l_text_err :: Expected String -> Found String -> { pos :: Int } -> ASTNode Int
l_text_err (Expected text) found { pos } =
  Tree.leaf { rule : Text text, result : Fail pos $ TextError { expected : Expected text, found : found }  }


l_text_eoi_err :: { pos :: Int } -> ASTNode Int
l_text_eoi_err { pos } =
  -- l_text_err (Expected "") EOI { pos }
  Tree.leaf { rule : Text "", result : Fail pos EndOfInput  }


n_ref :: String -> Range -> ASTNode Int -> ASTNode Int
n_ref ruleName range rulenode =
  Tree.node
    { rule : Ref Nothing ruleName, result : Match range 0 }
    [ rulenode ]


n_ref_capt :: { name :: String, as :: String } -> Range -> ASTNode Int -> ASTNode Int
n_ref_capt { name, as } range rulenode =
  Tree.node
    { rule : Ref (Just as) name, result : Match range 0 }
    [ rulenode ]


n_seq :: Range -> Array (ASTNode Int) -> ASTNode Int
n_seq range items =
  Tree.node
    { rule : Sequence $ ruleOf <$> items, result : Match range 0 }
    items


n_choice :: Range -> Array (ASTNode Int) -> ASTNode Int
n_choice range items =
  Tree.node
    { rule : Choice $ ruleOf <$> items, result : Match range 0 }
    items


n_rep_sep :: Range -> Array (ASTNode Int) -> ASTNode Int
n_rep_sep range items =
  Tree.node
    { rule : RepSep None None, result : Match range 0 }
    items


n_ref_err :: String -> { pos :: Int } -> ASTNode Int -> ASTNode Int
n_ref_err ruleName { pos } rulenode =
  Tree.node
    { rule : Ref Nothing ruleName, result : Fail pos $ RuleApplicationFailed { capture : Nothing, name : ruleName } }
    [ rulenode ]


n_seq_err :: { pos :: Int, entry :: Int } -> Array (ASTNode Int) -> ASTNode Int
n_seq_err { pos, entry } items =
  Tree.node
    { rule : Sequence $ ruleOf <$> items, result : Fail pos $ SequenceError { index : entry } }
    items


n_choice_err :: { pos :: Int } -> Array (ASTNode Int) -> ASTNode Int
n_choice_err { pos } items =
  Tree.node
    { rule : Choice $ ruleOf <$> items, result : Fail pos $ ChoiceError { } }
    items


n_rep_sep_err :: { pos :: Int, entry :: Int } -> Array (ASTNode Int) -> ASTNode Int
n_rep_sep_err { pos, entry } items =
  Tree.node
    { rule : RepSep None None, result : Fail pos $ RepSepHangingOperatorError { occurence : entry } }
    items


useGrammar :: String -> Either P.ParseError Grammar
useGrammar grammarStr = P.runParser grammarStr Grammar.parser


-- useGrammar' :: String -> Either P.ParseError Grammar
-- useGrammar' grammarStr = Right $ ?wh $ WithGrammar.parse Self.grammar (const unit) grammarStr


parsesGrammar ∷ ∀ (m ∷ Type -> Type). MonadThrow Ex.Error m ⇒ String → String → m Unit
parsesGrammar grammarStr expectation =
  (show <$> useGrammar grammarStr) `shouldEqual` (Right expectation)


parsesGrammarFile ∷ ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Ex.Error m ⇒ String → m Unit
parsesGrammarFile fileName = do
  grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
  expectation <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar.expected"
  let eGrammar = useGrammar grammarStr
  liftEffect $ writeTextFile Encoding.UTF8 ("./test/grammars/" <> fileName <> ".grammar.result") $ reportE eGrammar
  (show <$> eGrammar) `shouldEqual` (Right expectation)


failsToParseGrammarWithError ∷ ∀ (m ∷ Type -> Type). MonadThrow Ex.Error m ⇒ String → P.ParseError → m Unit
failsToParseGrammarWithError grammarStr error =
  (show <$> useGrammar grammarStr) `shouldEqual` (Left error)


mkParseError :: String -> Int -> Int -> Int -> P.ParseError
mkParseError err line column index = P.ParseError err $ P.Position { line, column, index }


parsesWithGivenGrammarAs :: ∀ (m ∷ Type -> Type) a. Show a => MonadThrow Ex.Error m ⇒ String → String -> ASTNode a → m Unit
parsesWithGivenGrammarAs str grammarStr expectation =
  let
    eGrammar = useGrammar grammarStr
    buildAst grammar = WithGrammar.parseBy (const 0) grammar str
  in (show <$> buildAst <$> lmap convertError eGrammar) `shouldEqual` (Right $ show $ AST { source : str, tree : expectation })


convertsGrammarToTree :: ∀ (m ∷ Type -> Type). MonadThrow Ex.Error m ⇒ String -> String → m Unit
convertsGrammarToTree grammarStr expectation =
  let
    eGrammar = useGrammar grammarStr
  in (showTree <$> Grammar.toTree <$> lmap convertError eGrammar) `shouldEqual` (Right expectation)


parsesWithGrammarFromFile :: ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Ex.Error m => String -> m Unit
parsesWithGrammarFromFile fileName = do
    grammarStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
    sourceStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src"
    expectationStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/sources/" <> fileName <> ".src.expected"
    let
      eGrammar = useGrammar grammarStr
      buildAst :: Grammar -> AST Int
      buildAst grammar = WithGrammar.parseBy (const 0) grammar sourceStr
      eAST = buildAst <$> lmap convertError eGrammar
    liftEffect $ writeTextFile Encoding.UTF8 ("./test/sources/" <> fileName <> ".src.result") $ reportE eAST
    (show <$> eAST) `shouldEqual` (Right expectationStr)


parsesGivenGrammarWithGrammar :: ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Ex.Error m => String -> String -> m Unit
parsesGivenGrammarWithGrammar grammarToParse expectationStr = do
    let
      buildAst :: AST String
      buildAst = map Chunk.content $ fillChunks $ WithGrammar.parse Self.grammar grammarToParse
    -- liftEffect $ writeTextFile Encoding.UTF8 ("./test/sources/" <> fileName <> ".src.result") $ reportE eAST
    (show $ Self.extract buildAst) `shouldEqual` expectationStr


parsesGrammarWithGrammar :: ∀ (m ∷ Type -> Type). MonadEffect m => MonadThrow Ex.Error m => String -> m Unit
parsesGrammarWithGrammar fileName = do
    grammarToParseStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar"
    expectationStr <- liftEffect $ readTextFile Encoding.UTF8 $ "./test/grammars/" <> fileName <> ".grammar.expected"
    let
      buildAst :: AST String
      buildAst = map Chunk.content $ fillChunks $ WithGrammar.parse Self.grammar grammarToParseStr
      grammar :: Grammar
      grammar = Self.extract buildAst
    liftEffect $ writeTextFile Encoding.UTF8 ("./test/grammars/" <> fileName <> ".grammar.result2") $ show grammar
    (show grammar) `shouldEqual` expectationStr



convertError :: P.ParseError -> SP.ParseError
convertError (P.ParseError error (P.Position { index })) = { error, pos : index }


reportE :: forall a err. Show err => Show a => Either err a -> String
reportE = case _ of
    Left error -> "error::" <> show error
    Right grammar -> show grammar