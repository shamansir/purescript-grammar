module Grammar.Parsing where

import Prelude

import Debug as Debug

import Grammar (Grammar, AST(..), Rule(..), CharRule(..), At(..), CharX(..), Error(..), Failure, RuleName, RuleSet, Range)
import Grammar (main, find, findIn, set, toChar, toRepr, found, expected, eoi) as G

import Control.Lazy (defer)

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (singleton, fromFoldable, uncons, head, snoc) as Array
import Data.String (take, length, drop) as String
import Data.String.CodeUnits (singleton, charAt) as String
import Data.List (List(..)) as List
import Data.Foldable (foldl, class Foldable)

-- import Parsing (Parser, ParseError, Position(..), runParser)
-- import Parsing (position, fail) as P
-- import Parsing.Combinators ((<?>))
-- import Parsing.Combinators as P
import StringParser (Parser(..), ParseError, PosString, runParser)
import StringParser (fail) as P
import StringParser.Combinators as P -- TODO!
import StringParser.CodePoints as P -- TODO!
import Parsing.String
-- import Parsing.Combinators.Array as PA
-- import Parsing.String as P
-- import Parsing.String.Basic as P


type P a = Parser a


parse :: forall a. Grammar -> (Rule -> a) -> String -> Either ParseError (AST a)
parse grammar f str =
    flip runParser str $ parseRule (G.set grammar) f Main $ G.main grammar


parseRule :: forall a. RuleSet -> (Rule -> a) -> At -> Rule -> P (AST a)
parseRule set f at rule =
    case rule of
        Text text -> qleaf rule $ P.string text
        CharRule (Single chx) -> qleaf rule $ P.char $ G.toChar chx
        CharRule (Not chx) -> qleaf rule $ do
            mbExclude <- P.optionMaybe $ P.try $ P.char $ G.toChar chx
            case mbExclude of
                Just x -> P.fail $ "found " <> show x
                Nothing -> P.anyChar *> pure unit
        CharRule (Range from to) ->
            qleaf rule
                $ do
                    {-
                    mbChar <- P.optionMaybe $ P.try $ P.anyChar
                    case mbChar of
                        Just ch ->
                            if (ch >= from) && (ch <= to) then
                                pure ch
                            else
                                P.fail $ show ch <> " is not from range " <> String.singleton from <> "-" <> String.singleton to
                        Nothing -> P.fail $ "didn't found char from range " <> String.singleton from <> "-" <> String.singleton to
                    -}
                    ch <- P.lookAhead $ P.anyChar
                    if (ch >= from) && (ch <= to) then
                        P.anyChar
                        -- P.advance $ pure ch -- FIXME
                    else P.fail $ show ch <> " is not from range " <> String.singleton from <> "-" <> String.singleton to
                --  $  isFromRange from to
                -- =<< P.anyChar
        CharRule Any -> qleaf rule P.anyChar
        Sequence rules ->
            qnode rule $ forWithIndex rules $ \idx -> parseRule set f $ InSequence idx
        Choice rules ->
            qnode rule $ Array.singleton <$>
                (_choice' failureFromState
                    $ map P.try
                    $ mapWithIndex
                        (\idx -> parseRule set f $ ChoiceOf idx) rules)
        Ref mbCapture ruleName ->
            case G.findIn set ruleName of
                Just foundRule -> parseRule set f (At $ mbCapture # fromMaybe ruleName) foundRule
                Nothing -> P.fail $ "Rule " <> ruleName <> " was not found in grammar"
        RepSep rep sep ->
            let
                prep = parseRule set f RepOf rep
                psep = parseRule set f SepOf sep
            in
                -- qnode $ Array.fromFoldable <$> P.sepEndBy prep psep
                qnode rule $ _repSep prep psep
        Placeholder ->
            qleaf rule $ P.string "???"
    where
        qleaf :: forall с. Rule -> P с -> P (AST a)
        qleaf = withRange leaf <<< flip mkLeafFailure
        qnode :: Rule -> P (Array (AST a)) -> P (AST a)
        qnode = withRange node <<< flip mkNodeFailure
        leaf :: forall x. x -> Range -> AST a
        leaf _ range = Leaf { at, rule, range } $ f rule
        node :: Array (AST a) -> Range -> AST a
        node rules range = Node { at, rule, range } (f rule) rules
        withRange :: forall c z. (c -> Range -> z) -> (PosString -> z) -> P c -> P z
        withRange mksucc mkfail p = do
            stateBefore <- _state
            mbResult <- P.optionMaybe $ P.try p
            stateAfter <- _state
            case mbResult of
                Just result ->
                    pure $ mksucc result { start : stateBefore.position, end : stateAfter.position }
                Nothing -> do
                    _advance $ advanceFor rule
                    pure $ mkfail stateBefore
        advanceFor :: Rule -> Int
        advanceFor = case _ of
            Text expected -> String.length expected
            CharRule _ -> 1
            Placeholder -> 3
            _ -> 0
        failureFromState :: PosString -> Failure
        failureFromState state = { position : state.position, rule, at, error : _makeError state.substring rule }
        mkLeafFailure :: PosString -> Rule -> AST a
        mkLeafFailure state = const $ FailLeaf $ failureFromState state
        mkNodeFailure :: PosString -> Rule -> AST a -- This should never occur when error-saving technique is used, since all parsers in the nodes formally succeed (just signalizing the fail with `Fail` constructor of `AST` datatype)
        mkNodeFailure state = const $ FailNode (failureFromState state) []


_makeError :: String -> Rule -> Error
_makeError substr =
    case _ of
        Text expected -> TextError { expected : G.expected expected, found : G.found $ String.take (String.length expected) substr }
        CharRule (Single chx) -> CharacterError { expected : G.expected chx, found : qfoundchar substr }
        CharRule (Not chx) -> NegCharacterError { notExpected : G.expected chx, found : qfoundchar substr }
        CharRule (Range from to) -> CharacterRangeError { from : G.expected from, to : G.expected to, found : qfoundchar substr }
        CharRule Any -> AnyCharacterError { found : qfoundchar substr }
        Choice _ -> ChoiceError { }
        Sequence _ -> SequenceError { }
        Ref _ name -> RuleNotFoundError { name }
        RepSep _ _ -> RepSepError { occurence : 0 } -- FIXME
        Placeholder -> PlaceholderError
    where
        qfoundchar = String.take 1 >>> String.charAt 0 >>> maybe G.eoi G.found -- FIXME: EOL/EOF


_choice :: forall a. (PosString -> Failure) -> Array (P (AST a)) -> P (AST a)
_choice toFailure = choiceIter [] 0 -- Array.scanl ??? FIXME
    where
        choiceIter :: Array (AST a) -> Int -> Array (P (AST a)) -> P (AST a) -- FIXME: return children array instead and wrap it in the AST node above
        choiceIter results idx items =
            case Array.uncons items of
                Just { head, tail } -> do
                    mbCurrent <- P.optionMaybe $ P.tryAhead head
                    let
                        continueBy result = do
                            choiceIter (result# Array.snoc results) (idx + 1) tail
                    case mbCurrent of
                        Just (FailLeaf failure) -> do
                            continueBy $ FailLeaf failure
                        Just (FailNode failure children) -> do
                            continueBy $ FailNode failure children
                        Just _ -> do
                            applied <- P.assertConsume head
                            pure applied
                        Nothing -> do
                            continueBy $ Nil
                Nothing -> do
                    curState <- _state
                    pure $ FailNode (toFailure curState) results


 -- duplicate of `choice`` which works the same way but with slightly different algorithm (`try` instead of `tryAhead` + `assertConsume`, but then rollbacks on failure), test speed
_choice' :: forall a. (PosString -> Failure) -> Array (P (AST a)) -> P (AST a)
_choice' toFailure = choiceIter [] 0 -- Array.scanl ??? FIXME
    where
        choiceIter :: Array (AST a) -> Int -> Array (P (AST a)) -> P (AST a) -- FIXME: return children array instead and wrap it in the AST node above
        choiceIter results idx items =
            case Array.uncons items of
                Just { head, tail } -> do
                    stateBefore <- _state
                    mbCurrent <- P.optionMaybe $ P.try head
                    let
                        continueBy result = do
                            _rollback stateBefore
                            choiceIter (result # Array.snoc results) (idx + 1) tail
                    case mbCurrent of
                        Just (FailLeaf failure) -> do
                            continueBy $ FailLeaf failure
                        Just (FailNode failure children) -> do
                            continueBy $ FailNode failure children
                        Just success -> do
                            pure success
                        Nothing -> do
                            continueBy $ Nil
                Nothing -> do
                    curState <- _state
                    pure $ FailNode (toFailure curState) results


_repSep :: forall a. P (AST a) -> P (AST a) -> P (Array (AST a))
_repSep rep sep = do
    repSepIter [] 0
    where
        repSepIter :: Array (AST a) -> Int -> P (Array (AST a))
        repSepIter results index = do
            mbRep <- P.optionMaybe $ P.tryAhead rep
            let
                stopWith lastFailure =
                    pure (lastFailure # Array.snoc results)
            case mbRep of
                Just (FailLeaf failure) ->
                    stopWith $ FailLeaf failure
                Just (FailNode failure children) ->
                    stopWith $ FailNode failure children
                Just repSuccess -> do
                    repApplied <- P.assertConsume rep
                    mbSep <- P.optionMaybe $ P.tryAhead sep
                    case mbSep of
                        Just (FailLeaf failure) ->
                            stopWith Nil
                            -- stopWith $ FailLeaf failure
                        Just (FailNode failure children) ->
                            stopWith Nil
                            -- stopWith $ FailNode failure children
                        Just sepSuccess -> do
                            _ <- P.assertConsume sep
                            repSepIter (repSuccess # Array.snoc results) $ index + 1
                        Nothing ->
                            stopWith Nil
                Nothing ->
                    stopWith Nil





_rollback :: PosString -> P Unit
_rollback state = Parser $ const $ pure { result : unit, suffix : state }


_advance :: Int -> P Unit
_advance howMuch =
    Parser $ \state ->
        let
            substrLen = String.length state.substring
            safeHowMuch = if substrLen < howMuch then substrLen else howMuch
        in
            pure
                { result : unit
                , suffix :
                    { position : state.position + safeHowMuch
                    , substring : String.drop safeHowMuch state.substring
                    }
                }


_state :: P PosString
_state = Parser \state -> pure { result : state, suffix : state }


_position :: P Int
_position = Parser \state -> pure { result : state.position, suffix : state }


{-
_repSep :: forall a sep. P a -> P sep -> P (Array a)
_repSep _ _ = defer \_ -> do
    pure []
-}