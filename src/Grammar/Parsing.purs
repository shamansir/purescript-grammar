module Grammar.Parsing where

import Prelude

import Grammar (Grammar, AST(..), Rule(..), CharRule(..), At(..), CharX(..), RuleName, RuleSet, Range)
import Grammar (main, find, findIn, set, toChar, toRepr) as G

import Control.Lazy (defer)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (singleton, fromFoldable, uncons, head) as Array
import Data.String (take, length, drop) as String
import Data.String.CodeUnits (singleton) as String
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
            qnode rule $ Array.singleton <$> (_choice Nil $ map P.try $ mapWithIndex (\idx -> parseRule set f $ ChoiceOf idx) rules)
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
                qnode rule $ Array.fromFoldable <$> (do
                    la <- P.optionMaybe $ P.lookAhead prep
                    case la of
                        Just _ -> P.sepEndBy prep psep
                        Nothing -> pure List.Nil
                    )
        Placeholder ->
            qleaf rule $ P.string "???"
    where
        qleaf :: forall с. Rule -> P с -> P (AST a)
        qleaf = withRange leaf <<< flip mkFailure
        qnode :: Rule -> P (Array (AST a)) -> P (AST a)
        qnode = withRange node <<< flip mkFailure
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
        mkErrorMessage :: String -> Rule -> String
        mkErrorMessage substr = case _ of
            Text expected -> "Expected '" <> expected <> "', but found '" <> String.take (String.length expected) substr <> "'"
            CharRule (Single chx) -> "Expected '" <> G.toRepr chx <> "', but found '" <> String.take 1 substr <> "'"
            CharRule (Not chx) -> "Expected not to find '" <> G.toRepr chx <> "', but found '" <> String.take 1 substr <> "'"
            CharRule (Range from to) -> "Expected character in range from '" <> String.singleton from <> "' to '" <> String.singleton to <> "', but found '" <> String.take 1 substr <> "'"
            CharRule Any -> "Expected any character, but found '" <> String.take 1 substr <> "'"
            Choice _ -> "choice TODO"
            Sequence _ -> "sequence TODO"
            Ref _ _ -> "ref TODO"
            RepSep _ _ -> "repsep TODO"
            Placeholder -> "PLC"
        mkFailure :: PosString -> Rule -> AST a
        mkFailure state = const $ Fail { position : state.position, rule, at, error : mkErrorMessage state.substring rule }
        -- isFromRange :: Char -> Char -> Char -> P Char
        -- isFromRange from to ch =
        --     if (ch >= from) && (ch <= to)
        --         then pure ch
        --         else P.fail $ show ch <> " is not from range " <> String.singleton from <> "-" <> String.singleton to


_choice :: forall a. AST a -> Array (P (AST a)) -> P (AST a)
_choice fallback = choiceIter 0 -- Array.scanl ??? FIXME
    where
        choiceIter idx items =
            case Array.uncons items of
                Just { head, tail } -> do
                    stateBefore <- _state
                    mbCurrent <- P.optionMaybe $ P.try head
                    case mbCurrent of
                        Just (Fail failure) -> do
                            -- P.fail "aaa"
                            -- DO smth
                            choiceIter (idx + 1) tail
                        Just success -> pure success
                        Nothing -> do
                            _rollback stateBefore
                            choiceIter (idx + 1) tail
                Nothing -> pure fallback


_rollback :: forall a. PosString -> P Unit
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