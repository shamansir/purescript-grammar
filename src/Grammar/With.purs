module Grammar.With where

import Prelude

import Grammar (Grammar, AST(..), Rule(..), CharRule(..), RuleName, RuleSet, Range)
import Grammar (main, find, findIn, set) as G

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.Array (singleton, fromFoldable) as Array

import Parsing (Parser, ParseError, runParser)
import Parsing (position, fail) as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String as P
import Parsing.String.Basic as P


type P a = Parser String a


parse :: forall a. Grammar -> (Rule -> a) -> String -> Either ParseError (AST a)
parse grammar f str =
    runParser str $ parseRule (G.set grammar) f "main" $ G.main grammar


parseRule :: forall a. RuleSet -> (Rule -> a) -> RuleName -> Rule -> P (AST a)
parseRule set f rname rule =
    case rule of
        Text text -> ql $ P.string text
        CharRule (Single ch) -> ql $ P.char ch
        Sequence rules ->
            qn $ for rules $ parseRule set f "ch"
        Choice rules ->
            qn $ Array.singleton <$> (P.choice $ parseRule set f "ch" <$> rules)
        Ref mbCapture ruleName ->
            case G.findIn set ruleName of
                Just rule -> parseRule set f (mbCapture # fromMaybe ruleName) rule
                Nothing -> P.fail $ "Rule " <> ruleName <> " was not found in grammar"
        RepSep rep sep ->
            let
                prep = parseRule set f "rep" rep
                psep = parseRule set f "sep" sep
            in
                qn $ Array.fromFoldable <$> P.sepBy1 prep psep
        _ -> pure Nil
    where
        ql :: forall z. P z -> P (AST a)
        ql = withRange leaf
        qn :: P (Array (AST a)) -> P (AST a)
        qn = withRange node
        leaf :: forall x. x -> Range -> AST a
        leaf _ rng = Leaf rname rule rng $ f rule
        node :: Array (AST a) -> Range -> AST a
        node rules rng = Node rname rule rng (f rule) rules
        withRange :: forall c z. (c -> Range -> z) -> P c -> P z
        withRange frng p = do
            posBefore <- P.position
            res <- p
            posAfter <- P.position
            pure $ frng res { start : 0, end : 0 }
