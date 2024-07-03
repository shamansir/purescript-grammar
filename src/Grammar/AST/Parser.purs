module Grammar.AST.Parser where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Map (lookup) as Map
import Data.String (length) as String
import Data.String.CodeUnits (charAt, splitAt) as SCU
-- import Data.String.CodePoints (head) as SCP
import Data.Array (uncons, snoc, singleton) as Array
import Data.Tuple.Nested ((/\))

import Yoga.Tree.Extended (leaf, node, value) as Tree
import Grammar (Grammar, Rule(..), RuleSet, WhichChar(..), CharX, CaptureName, RuleName)
import Grammar (set, main, toChar) as G
import Grammar.AST (AST(..), ASTNode, Attempt(..), At(..), Error(..), Found(..))
import Grammar.AST (found, expected, eoi) as G


type State = { position :: Int, next :: String }


type Step a x = { value :: x, rest :: State, node :: ASTNode a }


newtype Parser a x = Parser (State -> Step a x)


type P a = Parser a Unit


type PX a x = Parser a x


{- TODO
type Context = Unit


type Processors =
    { init :: Context
    , pre :: Rule a -> Context -> Rule a
    , post :: Rule a -> Context -> ASTNode a -> Rule a
    , resolve :: Rule a -> ASTNode a -> Context -> Point a -> Point a
    }
-}


parse :: Grammar -> String -> AST Unit
parse = parseBy $ const unit


parseBy :: forall a. (Rule -> a) -> Grammar -> String -> AST a
parseBy f grammar = runParser $ parseRule (G.set grammar) f $ G.main grammar


runParser :: forall a. P a -> String -> AST a
runParser p str =
    runParserWith p { position : 0, next : str }


runParserWith :: forall a. P a -> State -> AST a
runParserWith p state =
    -- AST <<< _.node <<< step p
    AST
        { source : state.next
        , tree : _.node $ step p state
        }


step :: forall a. P a -> State -> Step a Unit
step (Parser p) = p


parseRule :: forall a. RuleSet -> (Rule -> a) -> Rule -> P a
parseRule set f rule =
    case rule of
        Text expected -> _tryLeaf f rule $ _text expected
        Char (Single charX) -> _tryLeaf f rule $ _char charX
        Char (Not charX) -> _tryLeaf f rule $ _negChar charX
        Char (Range from to) -> _tryLeaf f rule $ _charRange { from, to }
        Char Any -> _tryLeaf f rule $ _anyChar
        Sequence sequence -> _tryNode set f rule $ _sequence sequence
        Choice options -> _tryNode set f rule $ _choice options
        Ref mbCapture ruleName -> _tryNode set f rule $ _ref mbCapture ruleName
        RepSep rep sep -> _tryNode set f rule $ _repSep { rep, sep }
        Placeholder -> _tryLeaf f rule $ _text "???"
        None -> _unexpectedFail


type LeafParser x = { advance :: Int, tryWith :: Found x -> IfProceed }


type NodeParser rem a = { startWith :: rem, step :: Iteration rem a → IterStep rem a }


_text :: String -> LeafParser String
_text expected =
    { advance : String.length expected
    , tryWith : case _ of
        Found found ->
            if (found == expected)
                then Proceed
                else Stop $ TextError { expected : G.expected expected, found : G.found found }
        EOI ->
            if (String.length expected == 0) then Proceed
            else Stop $ TextError { expected : G.expected expected, found : G.eoi }
    }


_char :: CharX -> LeafParser String
_char expected = _ch_convert $ case _ of
    Found char ->
        if char == G.toChar expected
            then Proceed
        else Stop $ CharacterError { expected : G.expected expected, found : G.found char }
    EOI ->
        Stop $ CharacterError { expected : G.expected expected, found : G.eoi }


_anyChar :: LeafParser String
_anyChar =
    _ch_convert $ case _ of
        Found _ ->
            Proceed
        EOI ->
            Stop $ AnyCharacterError { found : G.eoi }


_negChar :: CharX -> LeafParser String
_negChar notExpected =
    _ch_convert $ case _ of
        Found char ->
            if char == G.toChar notExpected
                then Stop $ NegCharacterError { notExpected : G.expected notExpected, found : G.found char }
            else Proceed
        EOI ->
            Stop $ NegCharacterError { notExpected : G.expected notExpected, found : G.eoi }


_charRange :: { from :: Char, to :: Char } -> LeafParser String
_charRange { from, to } =
    _ch_convert $ case _ of
        Found char ->
            if (char >= from) && (char <= to)
                then Proceed
                else Stop $ CharacterRangeError { from : G.expected from, to : G.expected to, found : G.found char }
        EOI ->
            Stop $ CharacterRangeError { from : G.expected from, to : G.expected to, found : G.eoi }


_sequence :: forall a. Array Rule -> NodeParser (Array Rule) a
_sequence seq =
    { startWith : seq
    , step : \{ ctx, index, remaining, prev, soFar, irule } ->
        case Array.uncons remaining /\ prev of
            Just { head, tail } /\ Start -> -- first iteration, remaining sequence is not empty
                IterNext tail head (InSequence index) ctx.start

            Just { head, tail } /\ Prev pidx prevStep ->
                if not $ _failed prevStep.node then -- continue while matching, advance the state
                    IterNext tail head (InSequence index) prevStep.rest
                else -- stop with the failure when rule failed
                    IterStop prevStep.rest soFar
                        $ Fail (_.position prevStep.rest) $ SequenceError { index : pidx }

            Nothing /\ Prev pidx prevStep ->
                IterStop prevStep.rest soFar $ if not $ _failed prevStep.node then -- the last item matched, all is good
                    Match { start : ctx.start.position, end : _.position prevStep.rest } $ ctx.f irule
                else -- the sequence is finished, but the last rule failed to match, fail
                    Fail (_.position prevStep.rest) $ SequenceError { index : pidx }

            Nothing /\ Start -> -- when there are no rules specfieid in a sequence, it always matches as empty
                IterStop ctx.start soFar $ Match { start : ctx.start.position, end : ctx.start.position } $ ctx.f irule
    }


_choice :: forall a. Array Rule -> NodeParser (Array Rule) a
_choice options =
    { startWith : options
    , step : \{ ctx, index, remaining, prev, soFar, irule } ->
        case Array.uncons remaining /\ prev of
            Just { head, tail } /\ Start -> -- first iteration, remaining options are not empty
                IterNext tail head (ChoiceOf index) ctx.start

            remainingOptions /\ Prev _ prevStep -> -- following iteration, we check later if there are any options remaining
                if not $ _failed prevStep.node then -- matched last option successfully, stop with informing about it
                    IterStop prevStep.rest soFar -- [ prevStep.node ] (if we only want to include the successful node)
                        $ Match { start : ctx.start.position, end : _.position prevStep.rest }
                        $ ctx.f irule
                else -- still not matched, try further while there are options
                    case remainingOptions of
                        Just { head, tail } -> -- remaining options are not yet empty
                            IterNext tail head (ChoiceOf index) ctx.start -- try further, return to the intitial state on the next iteration
                        Nothing -> -- out of options, but didn't match previously
                            IterStop ctx.start soFar $ Fail ctx.start.position $ ChoiceError { }

            Nothing /\ Start -> -- when there are no options specfieid in a choice, it always matches as empty
                IterStop ctx.start soFar $ Match { start : ctx.start.position, end : ctx.start.position } $ ctx.f ctx.rule
    }


data RepOrSep
    = Rep
    | Sep


_repSep :: forall a. { rep :: Rule, sep :: Rule } -> NodeParser RepOrSep a
_repSep { rep, sep } =
    { startWith : Rep
    , step : \{ ctx, remaining, prev, soFar, irule } ->
        case remaining /\ prev of
            Rep /\ Start ->
                IterNext Sep rep RepOf ctx.start

            Sep /\ Start ->
                IterFail ctx.start -- should be impossible!

            repOrSep /\ Prev _ prevStep ->
                if not $ _failed prevStep.node then
                    case repOrSep of
                        Rep -> IterNext Sep rep RepOf prevStep.rest
                        Sep -> IterNext Rep sep SepOf prevStep.rest
                else
                    IterStop prevStep.rest soFar $ Match { start : ctx.start.position, end : _.position prevStep.rest } $ ctx.f irule
    }


_ref :: forall a. Maybe CaptureName -> RuleName -> NodeParser Unit a
_ref mbCapture ruleName =
    { startWith : unit
    , step : \{ ctx } ->
        let
            state = ctx.start
            mbRule = ctx.set # Map.lookup ruleName
            mbRuleStep =
                flip step state <$> parseRule ctx.set ctx.f <$> mbRule
            rest = maybe state _.rest mbRuleStep
            result =
                case mbRuleStep of
                    Just ruleStep ->
                        if (not $ _failed ruleStep.node) then
                            Match { start : state.position, end : _.position $ ruleStep.rest } $ ctx.f ctx.rule
                        else
                            Fail state.position $ RuleApplicationFailed { name : ruleName, capture : mbCapture }
                    Nothing -> Fail state.position
                                    $ maybe
                                        (RuleApplicationFailed { name : ruleName, capture : mbCapture })
                                        (const $ RuleNotFoundError { name : ruleName, capture : mbCapture })
                                    $ mbRule
            children = maybe [] (Array.singleton <<< _.node) mbRuleStep
        in
            IterStop rest children result

    }


data IfProceed
    = Proceed
    | Stop Error


data IterStep rem a
    = IterStop State (Array (ASTNode a)) (Attempt a)
    | IterNext rem Rule At State
    | IterFail State


data IterPrev a
    = Start
    | Prev Int (Step a Unit)


type IterContext a =
    { start :: State
    , rule :: Rule
    , f :: Rule -> a
    , set :: RuleSet
    }


type Iteration rem a =
    { index :: Int
    , remaining :: rem
    , soFar :: Array (ASTNode a)
    , irule :: Rule
    , prev :: IterPrev a
    , ctx :: IterContext a
    }


data AttemptsLimit
    = InfiniteAttempts
    | LimitedAttempts Int


_attemptsLimit :: AttemptsLimit
_attemptsLimit = LimitedAttempts 10000


_mayContinue :: AttemptsLimit -> Int -> Boolean
_mayContinue limit n = case limit of
    InfiniteAttempts -> true
    LimitedAttempts cmp -> n <= cmp


_tryLeaf :: forall a. (Rule -> a) -> Rule -> LeafParser String -> P a
_tryLeaf f rule lp = Parser \{ position, next } ->
    let
        { before, after } = SCU.splitAt lp.advance next
    in
        if String.length next > 0 then
            case lp.tryWith $ Found before of
                Proceed ->
                    _lmatch after position $ position + lp.advance
                Stop error ->
                    _lfail next position error
        else
            case lp.tryWith EOI of
                -- Proceed -> _lmatch after position position
                Proceed -> _lfail next position EndOfInput
                Stop error -> _lfail next position error

    where

        _lmatch :: String -> Int -> Int -> Step a Unit
        _lmatch nextStr curPos nextPos =
            { value: unit
            , rest: { next: nextStr, position: nextPos }
            , node :
                Tree.leaf
                    { rule
                    , result : Match { start : curPos, end : nextPos } $ f rule
                    }
            }

        _lfail :: String -> Int -> Error -> Step a Unit
        _lfail next position error =
            { value: unit
            , rest: { position, next }
            , node :
                Tree.leaf
                    { rule
                    , result : Fail position error
                    }
            }


_tryNode :: forall rem a. RuleSet -> (Rule -> a) -> Rule -> NodeParser rem a -> P a
_tryNode set f rule np =
    Parser \state ->
        iterStep state [] 0 $ np.step $ first state
    where
        iterStep :: State -> Array (ASTNode a) -> Int -> IterStep rem a -> Step _ Unit
        iterStep _ _ _ (IterFail state) = _unexpectedStep state
        iterStep _ _ _ (IterStop state children result) =
            { value : unit
            , rest : state
            , node : Tree.node { rule, result } children
            }
        iterStep start resultsSoFar index (IterNext remaining nextRule at state) =
            if _mayContinue _attemptsLimit index then
                let
                    ctx = mkctx start
                    ruleParser = parseRule set f nextRule
                    lastStep = step ruleParser state
                    soFar = Array.snoc resultsSoFar lastStep.node
                    nextIndex = index + 1 -- TODO: limit the number of attempts to prevent stack overflow
                in
                    iterStep start soFar nextIndex $ np.step $ next ctx nextIndex soFar remaining nextRule $ Prev index lastStep
            else
                _reachedAttemptsLimit state

        mkctx start = { start, rule, f, set }

        first :: State -> Iteration rem a
        first start = { index : 0, remaining : np.startWith, soFar : [], prev : Start, irule : None, ctx : mkctx start }

        next :: IterContext a -> Int -> Array (ASTNode a) -> rem -> Rule -> IterPrev a -> Iteration rem a
        next ctx nextIndex soFar remaining nextRule prev = { index : nextIndex, remaining, soFar, prev, irule : nextRule, ctx }


_ch_convert :: (Found Char -> IfProceed) -> LeafParser String
_ch_convert tryWith =
    { advance : 1
    , tryWith : case _ of
        Found found ->
            case SCU.charAt 0 found of
                Just char ->
                    tryWith $ Found char
                Nothing ->
                    tryWith EOI
        EOI ->
            tryWith EOI
    }



-- text :: forall a. a -> String -> P a
-- text a expected = _text (const a) (Text expected) expected



{- _state :: forall a. PX a State
_state =
    Parser $ \state -> { value : state, rest : state, node : Leaf { rule : None, result : Fail 0 EndOfInput } }


_try :: forall a x. PX a x -> P a
_try p =
    ?wh -}


_resultOf :: forall a. ASTNode a -> Attempt a
_resultOf = Tree.value >>> _.result


_failed :: forall a. ASTNode a -> Boolean
_failed = _resultOf >>> _failedAttempt


_failedAttempt :: forall a. Attempt a -> Boolean
_failedAttempt (Match _ _) = false
_failedAttempt (Fail _ _) = true


_unexpectedFail :: forall a. P a
_unexpectedFail =
    _unexpectedFail' unit


_unexpectedFail' :: forall a x. x -> PX a x
_unexpectedFail' value =
    Parser $ _unexpectedStep' value


_unexpectedStep :: forall a. State -> Step a Unit
_unexpectedStep =
    _unexpectedStep' unit


_unexpectedStep' :: forall a x. x -> State -> Step a x
_unexpectedStep' value state =
    { value, rest : state, node : Tree.leaf { rule : None, result : Fail 0 EndOfInput } }


_reachedAttemptsLimit :: forall a.  State -> Step a Unit
_reachedAttemptsLimit =
    _reachedAttemptsLimit' unit


_reachedAttemptsLimit' :: forall a x. x -> State -> Step a x
_reachedAttemptsLimit' value state =
    { value, rest : state, node : Tree.leaf { rule : None, result : Fail state.position ReachedAttemptsLimit } }