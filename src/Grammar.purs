module Grammar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, toUnfoldable) as Map
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..))
import Data.Tuple (uncurry) as Tuple


data Grammar = Grammar Rule (Map RuleName Rule)


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


data AST a
    = Nil
    | Node a (Array (AST a))
    | Leaf RuleName Rule Range a



data Failure = Failure Range



empty :: Grammar
empty = Grammar Placeholder Map.empty


from :: Rule -> Map RuleName Rule -> Grammar
from = Grammar


parse :: forall a. Grammar -> (Rule -> a) -> String -> AST a
parse _ _ _ = Nil


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
        Node a [] -> "{{" <> show a <> "}}"
        Node a children -> "{{" <> show a <> "}}:[[" <> String.joinWith "," (show <$> children) <> "]]"
        Leaf ruleName _ range a ->
            "<<" <> show range <> "{{" <> show a <> "}}" <> show ruleName <> ">>"