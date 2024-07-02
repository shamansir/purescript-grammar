module Grammar.AST where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton, slice) as String
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Array (catMaybes, find, index) as Array
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree.Extended (Tree)
import Yoga.Tree.Extended (break, leaf, value, children) as Tree

import Grammar (Rule(..), WhichChar(..), RuleName, CharX, CaptureName)
import Grammar (expands, toRepr) as G

import Grammar.AST.Point (Point)
import Grammar.AST.Point (Point(..)) as P


type Range = { start :: Int, end :: Int }


type Position = Int


data Attempt a -- TODO: Attempt f a, f is for failure value
    = Match Range a
    | Fail Position Error


derive instance Functor Attempt


type Cell a =
    { rule :: Rule, result :: Attempt a }


type ASTNode a =
    Tree (Cell a)


newtype AST a =
    AST { source :: String, tree :: ASTNode a }


derive instance Newtype (AST a) _
derive instance Functor AST


-- TODO: Merge `Found` and `Expected`, EndOfInput could also be expected


data Found a
    = Found a
    | EOI


newtype Expected a
    = Expected a


data At -- do we need it now? expose `locate` if we doe
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
    show = showTree Main <<< _.tree <<< unwrap
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
                Char _ -> Nothing
                Placeholder -> Nothing
                None -> Nothing

        _ruleType :: Rule -> String
        _ruleType = case _ of
            Sequence _ -> "seqnc"
            Choice _ -> "choice"
            Ref mbCapture name -> "ref:" <> fromMaybe name mbCapture
            Text _ -> "text"
            Char r -> case r of
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


empty :: forall a. AST a
empty = AST { source : "", tree : Tree.leaf { rule : None, result : Fail 0 Unknown } }


found :: forall a. a -> Found a
found = Found


expected :: forall a. a -> Expected a
expected = Expected


eoi :: forall a. Found a
eoi = EOI


root :: forall a. AST a -> ASTNode a
root = unwrap >>> _.tree


ruleOf :: forall a. ASTNode a -> Rule
ruleOf = Tree.value >>> _.rule


fillChunks :: forall a. AST a -> AST String
fillChunks = _over _fillChunks


rebuild :: forall a b. (Cell a -> Cell b) -> AST a -> AST b
rebuild f =
    _over $ const $ map f


_over :: forall a b. (String -> ASTNode a -> ASTNode b) -> AST a -> AST b
_over f = unwrap >>> (\{ source, tree } -> { source, tree : f source tree }) >>> wrap


mapBy :: forall a b. (Cell a -> b) -> AST a -> AST b
mapBy f =
    rebuild $ \cell -> let b = f cell in { rule : cell.rule, result : b <$ cell.result }


match :: forall a. Cell a -> Maybe a
match = _.result >>> case _ of
    Match _ a -> Just a
    Fail _ _ -> Nothing


toPoint :: forall a. ASTNode a -> Maybe (Point a)
toPoint node =
    case _.result $ Tree.value node of
        Match _ a ->
            case _.rule $ Tree.value node  of
                Sequence _ -> Just $ P.Many a $ Array.catMaybes $ (toPoint <$> Tree.children node)
                Choice _ ->
                    Tree.children node
                        # mapWithIndex (/\)
                        # Array.find (Tuple.snd >>> _isMatch)
                        <#> map toPoint
                        >>= \(n /\ mbMatch) -> mbMatch <#> P.OneOf a n
                RepSep _ _ ->
                    Just
                        $ P.Many a
                        $ Array.catMaybes
                        $ mapWithIndex
                            (\idx mbMatch -> if idx `mod` 2 == 1 then Nothing else mbMatch)
                            (toPoint <$> Tree.children node)
                Ref mbCapture ruleName ->
                    Array.index (Tree.children node) 0
                        >>= toPoint
                        <#> P.Rule a (fromMaybe ruleName mbCapture)
                Text _ -> Just $ P.Value a
                Char _ -> Just $ P.Value a
                Placeholder -> Nothing
                None -> Nothing
        Fail _ _ -> Nothing





_isMatch :: forall a. ASTNode a -> Boolean
_isMatch node = case _.result $ Tree.value node of
    Match _ _ -> true
    Fail _ _ -> false


_fillChunks :: forall a. String -> ASTNode a -> ASTNode String
_fillChunks from = map \x -> x { result = injectMatch x.result }
    where
        injectMatch :: Attempt a -> Attempt String
        injectMatch = case _ of
            Match rng _ -> Match rng $ String.slice rng.start rng.end from
            Fail pos error -> Fail pos error





{-
collectMatches :: forall a. String -> AST a -> Tree { rule :: String, match :: String }
collectMatches from = unwrap >>> _collectMatches from


-- FIXME: could remove children that itself are no rules, but contain other rules inside
-- TODO: maybe use YZ.flattenLocDepthFirst
_collectMatches :: forall a. String -> ASTNode a -> Tree { rule :: String, match :: String }
_collectMatches from = map convert >>> Tree.catMaybes { rule : "", match : "" }
    where
        tryAttempt :: Attempt a -> Maybe String
        tryAttempt = case _ of
            Match rng _ -> Just $ String.slice rng.start rng.end from
            Fail _ _ -> Nothing
        convert :: { rule :: Rule, result :: Attempt a } -> Maybe { rule :: String, match :: String }
        convert { rule, result } =
            tryAttempt result >>= \match -> case rule of
                Ref mbCapture ruleName ->
                    Just { rule : fromMaybe ruleName mbCapture, match }
                _ -> Nothing
-}