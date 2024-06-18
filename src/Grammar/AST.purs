module Grammar.AST where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton) as String
import Data.Newtype (class Newtype, unwrap)

import Grammar (Rule(..), CharRule(..), RuleName, CharX, CaptureName)
import Grammar (expands, toRepr) as G
import Grammar.AST.Tree (Tree)
import Grammar.AST.Tree (leaf, node, value, break) as Tree


type Range = { start :: Int, end :: Int }


type Position = Int


data Attempt a
    = Match Range a
    | Fail Position Error


type ASTNode a =
    Tree { rule :: Rule, result :: Attempt a }


newtype AST a =
    AST (ASTNode a)


derive instance Newtype (AST a) _


-- TODO: Merge `Found` and `Expected`, EndOfInput could also be expected


data Found a
    = Found a
    | EOI


newtype Expected a
    = Expected a


data At
    = Main
    | At RuleName
    | InSequence Int
    | ChoiceOf Int
    | RepOf
    | SepOf
    | Nil


data Error
    = TextError { expected :: Expected String, found :: Found String }
    | CharacterError { expected :: Expected CharX, found :: Found Char }
    | NegCharacterError { notExpected :: Expected CharX, found :: Found Char }
    | CharacterRangeError { from :: Expected Char, to :: Expected Char, found :: Found Char }
    | AnyCharacterError { found :: Found Char }
    | RuleNotFoundError { name :: RuleName, capture :: Maybe CaptureName }
    | RuleApplicationFailed { name :: RuleName, capture :: Maybe CaptureName }
    -- | RepeatError { occurence :: Int } -- { occurence :: Int, rep :: Error }
    -- | SeparatorError { occurence :: Int } -- { occurence :: Int, sep :: Error }
    | RepSepHangingOperatorError { occurence :: Int }
    | SequenceError { index :: Int } -- { errors :: Array Error }
    | ChoiceError {} -- { errors :: Array Error }
    | PlaceholderError
    | EndOfInput
    | ReachedAttemptsLimit
    | Unknown


instance Show a => Show (AST a) where
    show = showTree Main <<< unwrap
        where

        showTree :: At -> ASTNode a -> String
        showTree at =
            Tree.break $ \{rule, result} ->
                case _ of
                    [] ->
                        case result of
                            Match range a ->
                                if G.expands rule then
                                    "( " <> show a <> " " <> smatch { at, range, rule } <> " | " <> "∅" <> " )"
                                else
                                     "( " <> show a <> " " <> smatch { at, range, rule } <> " )"
                            Fail position error ->
                                if G.expands rule then
                                    "< " <> sfailure { error, at, rule, position } <> " | " <> "∅" <> " >"
                                else
                                    "< " <> sfailure { error, at, rule, position } <> " >"
                    children ->
                        case result of
                            Match range a ->
                                "( " <> show a <> " " <> smatch { at, range, rule } <> " | " <> schildren rule children <> " )"
                            Fail position error ->
                                "< " <> sfailure { error, at, rule, position } <> " | " <> schildren rule children <> " >"
        schildren rule children =
            String.joinWith " : " (mapWithIndex (showTree <<< fromMaybe Nil <<< flip _locate rule) children)
        smatch { at, range, rule } =
            show at <> " " <> _ruleType rule <> " " <> show range.start <> "-" <> show range.end
        sfailure { error, at, rule, position } =
            show error <> " :: " <> show at <> " " <> _ruleType rule <> " @" <> show position

        _locate :: Int -> Rule -> Maybe At
        _locate index =
            case _ of
                Sequence _ -> Just $ InSequence index
                Choice _ -> Just $ ChoiceOf index
                RepSep _ _ -> case index `mod` 2 of
                    0 -> Just RepOf
                    1 -> Just $ SepOf
                    _ -> Nothing
                Ref _ rule -> Just $ At rule
                Text _ -> Nothing
                CharRule _ -> Nothing
                Placeholder -> Nothing
                None -> Nothing

        _ruleType :: Rule -> String
        _ruleType = case _ of
            Sequence _ -> "seqnc"
            Choice _ -> "choice"
            Ref mbCapture name -> "ref:" <> fromMaybe name mbCapture
            Text _ -> "text"
            CharRule r -> case r of
                Single _ -> "char"
                Any -> "any"
                Not _ -> "not-char"
                Range _ _ -> "char-range"
            RepSep _ _ -> "repsep"
            Placeholder -> "-"
            None -> "x"


instance Show Error where
    show = case _ of
        TextError err -> "Expected " <> show err.expected <> ", but found " <> show err.found
        CharacterError err -> "Expected " <> show err.expected <> ", but found " <> show err.found
        NegCharacterError err -> "Expected not to find " <> show err.notExpected <> ", but found " <> show err.found
        CharacterRangeError err -> "Expected character in range from " <> show err.from <> " to " <> show err.to <> ", but found " <> show err.found
        AnyCharacterError err -> "Expected any character, but found " <> show err.found
        ChoiceError _ -> "None of choices matched input"
        RuleNotFoundError err -> "Rule `" <> err.name <> "` was not found"
        RuleApplicationFailed err -> "Rule `" <> err.name <> "` didn't match"
        SequenceError { index } -> "Sequence failed at entry " <> show index
        -- RepeatError err -> "rep TODO"
        -- SeparatorError err -> "sep TODO"
        RepSepHangingOperatorError _ -> "Rep/Sep has hanging operator"
        EndOfInput -> "End of input"
        ReachedAttemptsLimit -> "Reached the limit of attempts"
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
        Expected chx -> "'" <> G.toRepr chx <> "'"


instance Show (Expected String) where
    show = case _ of
        Expected str -> "'" <> str <> "'"


instance Show At where
    show = case _ of
        Main -> "<main>"
        At ruleName -> "rule:" <> ruleName
        InSequence idx -> "seq:" <> show idx
        ChoiceOf idx -> "ch:" <> show idx
        RepOf -> "rep"
        SepOf -> "sep"
        Nil -> "-"


found :: forall a. a -> Found a
found = Found


expected :: forall a. a -> Expected a
expected = Expected


eoi :: forall a. Found a
eoi = EOI


ruleOf :: forall a. ASTNode a -> Rule
ruleOf = Tree.value >>> _.rule