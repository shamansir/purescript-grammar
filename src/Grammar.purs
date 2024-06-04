module Grammar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton) as String
import Data.Tuple (Tuple(..))
import Data.Tuple (uncurry) as Tuple


type RuleSet = Map RuleName Rule


data Grammar = Grammar Rule RuleSet


data Rule
    = Sequence (Array Rule)
    | Choice (Array Rule)
    | Ref (Maybe CaptureName) RuleName
    | Text String
    | CharRule CharRule
    | RepSep Rule Rule
    | Placeholder


data CharRule
    = Range Char Char
    | Not CharX
    | Single CharX
    | Any


data CharX
    = Escaped Char
    | Raw Char


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


type Match =
    { range :: Range -- in
    , match :: MatchAt -- parent
    , rule :: Rule
    }


data AST a
    = Nil
    | Leaf Match a
    | Node Match a (Array (AST a))


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


toChar :: CharX -> Char
toChar (Raw ch) = ch
toChar (Escaped ch) =
    case ch of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        'x' -> '\x'
        '\\' -> '\\'
        '"' -> '\"'
        -- ''' -> '''
        other -> other


instance Show CharRule where
    show = case _ of
        Range chA chB -> "[" <> String.singleton chA <> "-" <> String.singleton chB <> "]"
        Not char -> "^" <> show (Single char)
        Single char ->
            "'" <> (case char of
                        Escaped ch -> "\\" <> String.singleton ch
                        Raw ch -> String.singleton ch) <> "'"
        Any -> "."


instance Show Rule where
    show = case _ of
        Sequence rules -> "[" <> (String.joinWith "," $ show <$> rules) <> "]"
        Choice rules -> "(" <> (String.joinWith "|" $ show <$> rules) <> ")"
        Ref mbCapture ruleName ->
            case mbCapture of
                Just captureName -> captureName <> ":" <> ruleName
                Nothing -> ruleName
        Text text -> show text
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
        Node match a [] ->
            "( " <> show a <> " " <> smatch match <> " | " <> "∅" <> " )"
        Node match a children ->
            "( " <> show a <> " " <> smatch match <> " | " <> String.joinWith " : " (show <$> children) <> " )"
        Leaf match a ->
            "( " <> show a <> " " <> smatch match <> " )"
        where
            smatch { match, range, rule } =
                show match <> " " <> ruleType rule <> " " <> show range.start <> "-" <> show range.end
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