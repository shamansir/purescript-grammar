module Grammar.With where

import Prelude

import Grammar (Grammar, AST(..), Rule(..), CharRule(..), MatchAt(..), RuleName, RuleSet, Range)
import Grammar (main, find, findIn, set) as G

import Control.Lazy (defer)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (singleton, fromFoldable) as Array

import Parsing (Parser, ParseError, Position(..), runParser)
import Parsing (position, fail) as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String as P
import Parsing.String.Basic as P


type P a = Parser String a


parse :: forall a. Grammar -> (Rule -> a) -> String -> Either ParseError (AST a)
parse grammar f str =
    runParser str $ parseRule (G.set grammar) f Main $ G.main grammar


parseRule :: forall a. RuleSet -> (Rule -> a) -> MatchAt -> Rule -> P (AST a)
parseRule set f match rule =
    case rule of
        Text text -> qleaf $ P.string text
        CharRule (Single ch) -> qleaf $ P.char ch
        CharRule (Not ch) -> qleaf $ do
            mbExclude <- P.optionMaybe $ P.try $ P.char ch
            case mbExclude of
                Just x -> P.fail $ "found " <> show x
                Nothing -> P.anyChar *> pure unit
        CharRule (Range from to) ->
            qleaf $ pure unit -- FIXME: TODO
        CharRule Any -> qleaf P.anyChar
        Sequence rules ->
            qnode $ forWithIndex rules $ \idx -> parseRule set f $ InSequence idx
        Choice rules ->
            qnode $ Array.singleton <$> (P.choice $ mapWithIndex (\idx -> parseRule set f $ ChoiceOf idx) rules)
        Ref mbCapture ruleName ->
            case G.findIn set ruleName of
                Just rule -> parseRule set f (AtRule $ mbCapture # fromMaybe ruleName) rule
                Nothing -> P.fail $ "Rule " <> ruleName <> " was not found in grammar"
        RepSep rep sep ->
            let
                prep = parseRule set f RepOf rep
                psep = parseRule set f SepOf sep
            in
                qnode $ Array.fromFoldable <$> P.sepBy1 prep psep
                -- qnode $ _repSep prep psep
        Placeholder ->
            qleaf $ P.string "??"
    where
        qleaf :: forall z. P z -> P (AST a)
        qleaf = withRange leaf
        qnode :: P (Array (AST a)) -> P (AST a)
        qnode = withRange node
        leaf :: forall x. x -> Range -> AST a
        leaf _ range = Leaf { match, rule, range } $ f rule
        node :: Array (AST a) -> Range -> AST a
        node rules range = Node { match, rule, range } (f rule) rules
        withRange :: forall c z. (c -> Range -> z) -> P c -> P z
        withRange frng p = do
            (Position posBefore) <- P.position
            res <- p
            (Position posAfter) <- P.position
            pure $ frng res { start : posBefore.index, end : posAfter.index }


{-
_repSep :: forall a sep. P a -> P sep -> P (Array a)
_repSep _ _ = defer \_ -> do
    pure []
-}