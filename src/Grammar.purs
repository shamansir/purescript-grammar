module Grammar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..))
import Data.Tuple (uncurry) as Tuple


type RuleSet = Map RuleName Rule


data Grammar = Grammar Rule RuleSet


data CharRule
    = Range Char Char
    | Not Char
    | Single Char
    | Any


data Rule
    = Sequence (Array Rule)
    | Choice (Array Rule)
    | Ref (Maybe CaptureName) RuleName
    | Text String
    | CharRule CharRule
    | RepSep Rule Rule
    | Placeholder


type RuleName = String


type CaptureName = String



type Range = { start :: Int, end :: Int }


data MatchAt
    = Main
    | AtRule RuleName
    | InSequence Int
    | ChoiceOf Int
    | RepOf
    | SepOf


data AST a
    = Nil
    | Leaf MatchAt Rule Range a
    | Node MatchAt Rule Range a (Array (AST a))



data Failure = Failure Range



empty :: Grammar
empty = Grammar Placeholder Map.empty


from :: Rule -> Map RuleName Rule -> Grammar
from = Grammar


main :: Grammar -> Rule
main (Grammar m _) = m


find :: Grammar -> RuleName -> Maybe Rule
find = set >>> findIn


set :: Grammar -> RuleSet
set (Grammar _ s) = s


findIn :: RuleSet -> RuleName -> Maybe Rule
findIn = flip Map.lookup


instance Show CharRule where
    show = case _ of
        Range chA chB -> "[" <> show chA <> "-" <> show chB <> "]"
        Not char -> "^" <> show char -- "^'" <> show char <> "'"
        Single char -> show char -- "'" <> show char <> "'"
        Any -> "."


instance Show Rule where
    show = case _ of
        Sequence rules -> "[" <> (String.joinWith "," $ show <$> rules) <> "]"
        Choice rules -> "(" <> (String.joinWith "|" $ show <$> rules) <> ")"
        Ref mbCapture ruleName ->
            case mbCapture of
                Just captureName -> captureName <> ":" <> ruleName
                Nothing -> ruleName
        Text text -> "\"" <> text <> "\""
        CharRule chrule -> show chrule
        RepSep repRule sepRule ->
            -- TODO: Some operator for RepSep
            "repSep(" <> show repRule <> "," <> show sepRule <> ")"
        Placeholder -> "???"


instance Show Grammar where
    show (Grammar main others) =
        ruleLine "main" main <> "\n" <> String.joinWith "\n" (Map.toUnfoldable others <#> Tuple.uncurry ruleLine)
        where
            ruleLine name rule =
                name <> " :- " <> show rule <> "."


instance Show MatchAt where
    show = case _ of
        Main -> "<main>"
        AtRule ruleName -> "rule:" <> ruleName
        InSequence idx -> "seq:" <> show idx
        ChoiceOf idx -> "ch:" <> show idx
        RepOf -> "rep"
        SepOf -> "sep"



instance Show a => Show (AST a) where
    show = case _ of
        Nil -> "-"
        Node match rule range a [] ->
            "( " <> show a <> " " <> show match <> " " <> ruleType rule <> " " <> show range.start <> "-" <> show range.end <> " | " <> "∅" <> " )"
        Node match rule range a children ->
            "( " <> show a <> " " <> show match <> " " <> ruleType rule <> " " <> show range.start <> "-" <> show range.end <> " | " <> String.joinWith " : " (show <$> children) <> " )"
        Leaf match rule range a ->
            "( " <> show a <> " " <> show match <> " " <> ruleType rule <> " " <> show range.start <> "-" <> show range.end <> " )"
        where
            ruleType = case _ of
                Sequence _ -> "seqnc"
                Choice _ -> "choice"
                Ref _ name -> "ref:" <> name
                Text _ -> "text"
                CharRule r -> case r of
                    Single _ -> "char"
                    Any -> "any"
                    Not _ -> "not-char"
                    Range _ _ -> "char-range"
                RepSep _ _ -> "repsep"
                Placeholder -> "-"