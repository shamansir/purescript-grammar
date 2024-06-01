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


{-
data MatchAt
    = AtRule RuleName Rule
    | InSeqence MatchAt Int Rule
    | ChoiceOf MatchAt Int Rule
    | RepOf MatchAt Rule
    | SepOf MatchAt Rule
-}


data AST a
    = Nil
    | Leaf RuleName Rule Range a
    | Node RuleName Rule Range a (Array (AST a))



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



instance Show a => Show (AST a) where
    show = case _ of
        Nil -> "-"
        Node a _ _ _ [] -> "{{" <> show a <> "}}"
        Node a _ _ _ children -> "{{" <> show a <> "}}:[[" <> String.joinWith "," (show <$> children) <> "]]"
        Leaf ruleName _ range a ->
            "<<" <> show range <> "{{" <> show a <> "}}" <> show ruleName <> ">>"