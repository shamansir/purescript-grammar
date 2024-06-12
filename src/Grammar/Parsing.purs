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
-- import Data.String.CodePoints (eof) as String
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
    -- TODO: most of the parsers here use `P.fail` from the inside but the error result is then skipped and treated as failure on a higher level (e.g. leaf in the node)
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
                    ch <- P.lookAhead $ P.anyChar
                    if (ch >= from) && (ch <= to) then
                        P.anyChar
                        -- P.advance $ pure ch -- FIXME
                    else P.fail $ show ch <> " is not from range " <> String.singleton from <> "-" <> String.singleton to
                --  $  isFromRange from to
                -- =<< P.anyChar
        CharRule Any -> qleaf rule P.anyChar
        Sequence rules ->
            -- TODO: custom implementation with failure tracking
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


type Handlers_ a b = { onFail :: AST a -> P b, onSuccess :: AST a -> P b }


-- _tryAhead :: forall a b. Handlers_ a b -> P (AST a) -> P b
-- _tryAhead spec p = (P.tryAhead p # P.optionMaybe) >>= _mbHelper spec


-- _try :: forall a b. Handlers_ a b -> P (AST a) -> P b
-- _try spec p = (P.try p # P.optionMaybe) >>= _mbHelper spec


_tryAhead :: forall a b. Handlers_ a b -> P (AST a) -> P b
_tryAhead = _safeTry


_try :: forall a b. Handlers_ a b -> P (AST a) -> P b
_try = _safeTry


_mbHelper :: forall a b. Handlers_ a b -> Maybe (AST a) -> P b
_mbHelper spec =
    case _ of
        Just (FailLeaf failure) -> do
            spec.onFail $ FailLeaf failure
        Just (FailNode failure children) -> do
            spec.onFail $ FailNode failure children
        Just success -> do
            spec.onSuccess success
        Nothing -> do
            spec.onFail $ Nil


_choice :: forall a. (PosString -> Failure) -> Array (P (AST a)) -> P (AST a)
_choice toFailure = choiceIter [] 0 -- Array.scanl ??? FIXME
    where
        choiceIter :: Array (AST a) -> Int -> Array (P (AST a)) -> P (AST a) -- FIXME: return children array instead and wrap it in the AST node above
        choiceIter results idx items =
            case Array.uncons items of
                Just { head, tail } -> do
                    head # _tryAhead
                        { onFail :
                            \failure -> do
                                choiceIter (failure # Array.snoc results) (idx + 1) tail
                        , onSuccess : pure
                        }
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
                    head # _try
                        { onFail :
                            \failure -> do
                                choiceIter (failure # Array.snoc results) (idx + 1) tail
                        , onSuccess : pure
                        }
                Nothing -> do
                    curState <- _state
                    pure $ FailNode (toFailure curState) results


_repSep :: forall a. P (AST a) -> P (AST a) -> P (Array (AST a))
_repSep rep sep = do
    let _ = Debug.spy "start" unit
    rep # _tryAhead
        { onFail : pure <<< Array.singleton
        , onSuccess :
            \repSuccess -> do
                let _ = Debug.spy "first success" unit
                -- _ <- _ensureNotEOI
                -- _ <- P.assertConsume rep
                state <- _state
                let _ = Debug.spy "state after sep success" state
                repSepIter (Array.singleton repSuccess) 1
        }
    where
        repSepIter :: Array (AST a) -> Int -> P (Array (AST a))
        repSepIter results index = do
            sep # _tryAhead
                { onFail : \sepFailure -> pure $ Debug.spy ("fail sep at " <> show index) (sepFailure # Array.snoc results)
                , onSuccess :
                    \sepSuccess -> do
                        state <- _state
                        let _ = Debug.spy "state after sep success" state
                        rep # _tryAhead
                            { onFail :
                                \repFailure ->
                                    pure $ Debug.spy ("fail rep at " <> show index) (sepSuccess # Array.snoc (repFailure # Array.snoc results))
                            , onSuccess :
                                \repSuccess -> do
                                    let _ = Debug.spy "rep success" unit
                                    repSepIter (repSuccess # Array.snoc (sepSuccess # Array.snoc results)) $ index + 1
                                    -- pure (repSuccess # Array.snoc (sepSuccess # Array.snoc results))
                            }
                        -- pure (sepSuccess # Array.snoc (Nil # Array.snoc results))
                }
            -- pure results
            {-
            sep # _tryAhead
                { onFail :
                    \sepFailure -> pure $ Debug.spy ("fail sep at " <> show index) (sepFailure # Array.snoc results)
                , onSuccess : \sepSuccess -> do
                    let _ = Debug.spy "sep success" unit
                    -- isEOI <- _isEOI
                    -- let _ = Debug.spy "is EOI" isEOI
                    if (true) then do
                        let _ = Debug.spy "before assert consume" unit
                        -- FIXME: `assertConsume` does not assume empty consume as sucessful one
                        -- _ <- P.assertConsume sep -- do not consume if next rep fails?
                        let _ = Debug.spy "sep consume success" unit
                        rep # _tryAhead
                            { onFail :
                                \repFailure ->
                                    pure $ Debug.spy ("fail rep at " <> show index) (sepSuccess # Array.snoc (repFailure # Array.snoc results))
                            , onSuccess :
                                \repSuccess -> do
                                    let _ = Debug.spy "rep success" unit
                                    -- isEOI <- _isEOI
                                    -- let _ = Debug.spy "is EOI" isEOI
                                    if (true) then do
                                        -- _ <- P.assertConsume rep -- do not consume if next rep fails?
                                        let _ = Debug.spy "rep consume success"
                                        repSepIter (repSuccess # Array.snoc (sepSuccess # Array.snoc results)) $ index + 1
                                    else
                                        pure $ Debug.spy ("EOI at " <> show index) (sepSuccess # Array.snoc (Nil # Array.snoc results))
                            }
                    else
                        pure $ Debug.spy ("EOI at " <> show index) (sepSuccess # Array.snoc (Nil # Array.snoc results))
            -}


_safeTry :: forall a b. Handlers_ a b -> P (AST a) -> P b
_safeTry spec what = do
    stateBefore <- _state
    let _ = Debug.spy "state before try" stateBefore
    result <- _ensureNotEOIWith $ P.optionMaybe $ what
    stateAfter <- _state
    let _ = Debug.spy "state after try" stateAfter
    result # _mbHelper
        { onSuccess : spec.onSuccess
        , onFail : \res -> do
            _ <- _rollback stateBefore
            spec.onFail res
        }


{-
_safeTry' :: forall a. P (AST a) -> P (AST a)
_safeTry' what = do
    stateBefore <- _state
    result <- P.optionMaybe $ what
    result # _mbHelper
        { onSuccess : pure
        , onFail : \res -> do
            _ <- _rollback stateBefore
            pure res
        }
-}


_ensureNotEOIWith :: forall a. P (Maybe a) -> P (Maybe a)
_ensureNotEOIWith p = do
    state <- _state
    if (String.length state.substring == 0) then do
        let _ = Debug.spy "EOI" unit
        -- pure Nothing
        P.fail "EOI"
    else do
        res <- p
        pure res
        -- nextState <- _state
        -- if (String.length nextState.substring == 0) then
        --     Left { error : "EOI", pos : state.position }
        -- else
        --     Right { result : res, suffix : state }


{-
_ensureNotEOI = Parser $
    \state ->
        if (String.length state.substring == 0) then
            Left { error : "EOI", pos : state.position }
        else
            Right { result : unit, suffix : state }
-}


_isEOI :: P Boolean
_isEOI = Parser $
    \state ->
        Right { result : String.length state.substring == 0, suffix : state }


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