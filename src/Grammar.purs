module Grammar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
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


type RuleName = String


type CaptureName = String


type Range = { start :: Int, end :: Int }


data At
    = Main
    | At RuleName
    | InSequence Int
    | ChoiceOf Int
    | RepOf
    | SepOf


type Match =
    { range :: Range -- in
    , at :: At -- parent
    , rule :: Rule
    }


type Failure =
    { position :: Int
    , at :: At
    , rule :: Rule
    , error :: Error
    }


data AST a
    = Nil
    | Leaf Match a
    | Node Match a (Array (AST a))
    | FailLeaf Failure
    | FailNode Failure (Array (AST a)) -- Array Failure?


data Error
    = TextError { expected :: Expected String, found :: Found String }
    | CharacterError { expected :: Expected CharX, found :: Found Char }
    | NegCharacterError { notExpected :: Expected CharX, found :: Found Char }
    | CharacterRangeError { from :: Expected Char, to :: Expected Char, found :: Found Char }
    | AnyCharacterError { found :: Found Char }
    | RuleNotFoundError { name :: String }
    -- | RepeatError { occurence :: Int } -- { occurence :: Int, rep :: Error }
    -- | SeparatorError { occurence :: Int } -- { occurence :: Int, sep :: Error }
    | RepSepError { occurence :: Int }
    | SequenceError {} -- { errors :: Array Error }
    | ChoiceError {} -- { errors :: Array Error }
    | PlaceholderError
    | Unknown


data CharX
    = Escaped Char
    | Raw Char


-- TODO: Merge `Found` and `Expected`, EndOfInput could also be expected


data Found a
    = Found a
    | EOI


newtype Expected a
    = Expected a


{- TODO
advance :: Rule -> Error -> Int
-}


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


instance Show Grammar where
    show (Grammar main others) =
        ruleLine "main" main <> "\n" <> String.joinWith "\n" (Map.toUnfoldable others <#> Tuple.uncurry ruleLine)
        where
            ruleLine name rule =
                name <> " :- " <> show rule <> "."


instance Show At where
    show = case _ of
        Main -> "<main>"
        At ruleName -> "rule:" <> ruleName
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
        FailLeaf failure ->
            "< " <> sfailure failure <> " >"
        FailNode failure [] ->
            "< " <> sfailure failure <> " | " <> "∅" <> " >"
        FailNode failure children ->
            "< " <> sfailure failure <> " | " <> String.joinWith " : " (show <$> children) <> " >"
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
            smatch { at, range, rule } =
                show at <> " " <> ruleType rule <> " " <> show range.start <> "-" <> show range.end
            sfailure { error, at, rule, position } =
                show error <> " :: " <> show at <> " " <> ruleType rule <> " @" <> show position


instance Show Error where
    show = case _ of
        TextError err -> "Expected " <> show err.expected <> ", but found " <> show err.found
        CharacterError err -> "Expected " <> show err.expected <> ", but found " <> show err.found
        NegCharacterError err -> "Expected not to find " <> show err.notExpected <> ", but found " <> show err.found
        CharacterRangeError err -> "Expected character in range from " <> show err.from <> " to " <> show err.to <> ", but found " <> show err.found
        AnyCharacterError err -> "Expected any character, but found " <> show err.found
        ChoiceError _ -> "None of choices matched input"
        RuleNotFoundError err -> "Rule `" <> err.name <> "` was not found"
        SequenceError _ -> "sequence TODO"
        -- RepeatError err -> "rep TODO"
        -- SeparatorError err -> "sep TODO"
        RepSepError _ -> "repsep TODO"
        PlaceholderError -> "PLC"
        Unknown -> "UNK"


instance Show (Found Char) where
    show = case _ of
        Found ch -> "'" <> String.singleton ch <> "'"
        EOI -> "end-of-input"


instance Show (Found String) where
    show = case _ of
        Found str -> "'" <> str <> "'"
        EOI -> "end-of-input"


instance Show (Expected Char) where
    show = case _ of
        Expected ch -> "'" <> String.singleton ch <> "'"


instance Show (Expected CharX) where
    show = case _ of
        Expected chx -> "'" <> toRepr chx <> "'"


instance Show (Expected String) where
    show = case _ of
        Expected str -> "'" <> str <> "'"


found :: forall a. a -> Found a
found = Found


expected :: forall a. a -> Expected a
expected = Expected


eoi :: forall a. Found a
eoi = EOI