module Grammar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton) as String
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
    | None


data CharRule
    = Range Char Char
    | Not CharX
    | Single CharX
    | Any


type RuleName = String


type CaptureName = String


data CharX
    = Escaped Char
    | Raw Char


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


toRepr :: CharX -> String
toRepr (Raw ch) = String.singleton ch
toRepr (Escaped ch) =
    "\\" <> String.singleton ch


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
        None -> "x"


instance Show Grammar where
    show (Grammar main others) =
        ruleLine "main" main <> "\n" <> String.joinWith "\n" (Map.toUnfoldable others <#> Tuple.uncurry ruleLine)
        where
            ruleLine name rule =
                name <> " :- " <> show rule <> "."