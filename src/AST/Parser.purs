module AST.Parser where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Map (empty) as Map
import Data.String (length, take) as String
import Data.String.CodeUnits (charAt, length, splitAt, drop) as SCU
-- import Data.String.CodePoints (head) as SCP
import Data.Array (uncons, snoc, index) as Array
import Data.Tuple.Nested ((/\), type (/\))

import Grammar (Grammar, Rule(..), AST(..), Tree(..), Attempt(..), ASTNode, RuleSet, At(..), Error(..), CharRule(..), Found(..), Expected(..), CharX(..))
import Grammar (set, main, found, eoi, expected, toChar) as G


type State = { position :: Int, next :: String }


type Step a x = { value :: x, rest :: State, node :: ASTNode a }


newtype Parser a x = Parser (State -> Step a x)


type P a = Parser a Unit


type PX a x = Parser a x


parse :: forall a. Grammar -> (Rule -> a) -> String -> AST a
parse grammar f = runParser $ parseRule (G.set grammar) f Main $ G.main grammar


runParser :: forall a. P a -> String -> AST a
runParser p str =
    runParserWith p { position : 0, next : str }


runParserWith :: forall a. P a -> State -> AST a
runParserWith p =
    AST <<< _.node <<< step p


step :: forall a. P a -> State -> Step a Unit
step (Parser p) = p


parseRule :: forall a. RuleSet -> (Rule -> a) -> At -> Rule -> P a
parseRule set f at rule =
    case rule of
        Text expected -> _text f rule expected
        _ -> _unexpectedFail


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
    { value, rest : state, node : Leaf { rule : None, result : Fail 0 EndOfInput } }


data IfProceed
    = Proceed
    | Stop Error


_text :: forall a. (Rule -> a) -> Rule -> String -> P a
_text f rule expected =
    _ltry f rule (String.length expected) $ case _ of
        Found found ->
            if (found == expected)
                then Proceed
                else Stop $ TextError { expected : G.expected expected, found : G.found found }
        EOI ->
            Stop $ TextError { expected : G.expected expected, found : G.eoi }


_char :: forall a. (Rule -> a) -> Rule -> CharX -> P a
_char f rule expected =
    _ltryChar f rule $ case _ of
        Found char ->
            if char == G.toChar expected
                then Proceed
            else Stop $ CharacterError { expected : G.expected expected, found : G.found char }
        EOI ->
            Stop $ CharacterError { expected : G.expected expected, found : G.eoi }


_anyChar :: forall a. (Rule -> a) -> Rule -> P a
_anyChar f rule =
    _ltryChar f rule $ case _ of
        Found _ ->
            Proceed
        EOI ->
            Stop $ AnyCharacterError { found : G.eoi }


_negChar :: forall a. (Rule -> a) -> Rule -> CharX -> P a
_negChar f rule notExpected =
    _ltryChar f rule $ case _ of
        Found char ->
            if char == G.toChar notExpected
                then Stop $ NegCharacterError { notExpected : G.expected notExpected, found : G.found char }
            else Proceed
        EOI ->
            Stop $ NegCharacterError { notExpected : G.expected notExpected, found : G.eoi }


_charRange :: forall a. (Rule -> a) -> Rule -> Char -> Char -> P a
_charRange f rule from to =
    _ltryChar f rule $ case _ of
        Found char ->
            if (char >= from) && (char <= to)
                then Proceed
                else Stop $ CharacterRangeError { from : G.expected from, to : G.expected to, found : G.found char }
        EOI ->
            Stop $ CharacterRangeError { from : G.expected from, to : G.expected to, found : G.eoi }


_sequence :: forall a. RuleSet -> (Rule -> a) -> Rule -> Array Rule -> P a
_sequence set f rule seq =
    _niter set f rule seq $ \{ index, remaining, prev, soFar, start, irule } ->
        case Array.uncons remaining /\ prev of
            Just { head, tail } /\ Start -> -- first iteration, remaining sequence is not empty
                IterNext tail head (InSequence index) start

            Just { head, tail } /\ Prev prevStep ->
                if not $ _failed prevStep.node then -- continue while matching, advance the state
                    IterNext tail head (InSequence index) prevStep.rest
                else -- stop with the failure when rule failed
                    IterStop prevStep.rest soFar
                        $ Fail (_.position prevStep.rest) $ SequenceError { index }

            Nothing /\ Prev prevStep ->
                IterStop prevStep.rest soFar $ if not $ _failed prevStep.node then -- the last item matched, all is good
                    Match { start : start.position, end : _.position prevStep.rest } $ f irule
                else -- the sequence is finished, but the last rule failed to match, fail
                    Fail (_.position prevStep.rest) $ SequenceError { index }

            Nothing /\ Start -> -- when there are no rules specfieid in a sequence, it always matches as empty
                IterStop start soFar $ Match { start : start.position, end : start.position } $ f irule


_choice :: forall a. RuleSet -> (Rule -> a) -> Rule -> Array Rule -> P a
_choice set f rule options =
    _niter set f rule options $ \{ index, remaining, prev, soFar, start, irule } ->
        case Array.uncons remaining /\ prev of
            Just { head, tail } /\ Start -> -- first iteration, remaining options are not empty
                IterNext tail head (ChoiceOf index) start

            remainingOptions /\ Prev prevStep -> -- following iteration, we check later if there are any options remaining
                if not $ _failed prevStep.node then -- matched last option successfully, stop with informing about it
                    IterStop prevStep.rest [ prevStep.node ]
                        $ Match { start : start.position, end : _.position prevStep.rest }
                        $ f rule
                else -- still not matched, try further while there are options
                    case remainingOptions of
                        Just { head, tail } -> -- remaining options are not yet empty
                            IterNext tail head (ChoiceOf index) start -- try further, return to the intitial state on the next iteration
                        Nothing -> -- out of options, but didn't match previously
                            IterStop start soFar $ Fail start.position $ ChoiceError { }

            Nothing /\ Start -> -- when there are no options specfieid in a choice, it always matches as empty
                IterStop start soFar $ Match { start : start.position, end : start.position } $ f rule

data RepOrSep
    = Rep
    | Sep


_repSep :: forall a. RuleSet -> (Rule -> a) -> Rule -> { rep :: Rule, sep :: Rule } -> P a
_repSep set f rule { rep, sep } =
    _niter set f rule Rep $ \{ remaining, prev, soFar, start, irule } ->
        case remaining /\ prev of
            Rep /\ Start ->
                IterNext Sep rep RepOf start

            Sep /\ Start ->
                IterFail start -- should be impossible!

            repOrSep /\ Prev prevStep ->
                if not $ _failed prevStep.node then
                    case repOrSep of
                        Rep -> IterNext Sep rep RepOf prevStep.rest
                        Sep -> IterNext Rep sep SepOf prevStep.rest
                else
                    IterStop prevStep.rest soFar $ Match { start : start.position, end : _.position prevStep.rest } $ f irule


data IterStep rem a
    = IterStop State (Array (ASTNode a)) (Attempt a)
    | IterNext rem Rule At State
    | IterFail State


data IterPrev a
    = Start
    | Prev (Step a Unit)


type Iteration rem a =
    { index :: Int
    , remaining :: rem
    , soFar :: Array (ASTNode a)
    , start :: State
    , irule :: Rule
    , prev :: IterPrev a
    }


_niter :: forall rem a. RuleSet -> (Rule -> a) -> Rule -> rem -> (Iteration rem a -> IterStep rem a) -> P a
_niter set f rule remaining check =
    Parser \state ->
        iterStep state [] 0 $ check { index : 0, remaining, soFar : [], start : state, prev : Start, irule : None }
    where
        iterStep :: State -> Array (ASTNode a) -> Int -> IterStep rem a -> Step _ Unit
        iterStep _ _ _ (IterFail state) = _unexpectedStep state
        iterStep _ _ _ (IterStop state chilren result) =
            { value : unit
            , rest : state
            , node : Node { rule, result } chilren
            }
        iterStep start resultsSoFar index (IterNext remaining nextRule at state) =
            let
                ruleParser = parseRule set f at nextRule
                lastStep = step ruleParser state
                soFar = Array.snoc resultsSoFar lastStep.node
                nextIndex = index + 1 -- TODO: limit the number of attempts to prevent stack overflow
            in
                iterStep start soFar nextIndex $ check { index : nextIndex, remaining, soFar, start, prev : Prev lastStep, irule : nextRule }



_ltryChar :: forall a. (Rule -> a) -> Rule -> (Found Char -> IfProceed) -> P a
_ltryChar f rule tryWith = _ltry f rule 1 $ case _ of
    Found found ->
        case SCU.charAt 0 found of
            Just char ->
                tryWith $ Found char
            Nothing ->
                tryWith EOI
    EOI ->
        tryWith EOI


_ltry :: forall a. (Rule -> a) -> Rule -> Int -> (Found String -> IfProceed) -> P a
_ltry f rule advance tryWith = Parser \{ position, next } ->
    let
        { before, after } = SCU.splitAt advance next
    in
        if String.length next > 0 then
            case tryWith $ Found before of
                Proceed ->
                    _lmatch after position $ position + advance
                Stop error ->
                    _lfail next position error
        else
            case tryWith EOI of
                Proceed -> _lfail next position EndOfInput
                Stop error -> _lfail next position error

    where

        _lmatch :: String -> Int -> Int -> Step a Unit
        _lmatch nextStr curPos nextPos =
            { value: unit
            , rest: { next: nextStr, position: nextPos }
            , node :
                Leaf
                    { rule
                    , result : Match { start : curPos, end : nextPos } $ f rule
                    }
            }

        _lfail :: String -> Int -> Error -> Step a Unit
        _lfail next position error =
            { value: unit
            , rest: { position, next }
            , node :
                Leaf
                    { rule
                    , result : Fail position error
                    }
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
_resultOf (Leaf { result }) = result
_resultOf (Node { result } _) = result


_failed :: forall a. ASTNode a -> Boolean
_failed = _resultOf >>> _failedAttempt


_failedAttempt :: forall a. Attempt a -> Boolean
_failedAttempt (Match _ _) = false
_failedAttempt (Fail _ _) = true


_makeError :: String -> Rule -> Error
_makeError substr =
    case _ of
        Text expected -> TextError { expected : G.expected expected, found : qfoundstr expected }
        CharRule (Single chx) -> CharacterError { expected : G.expected chx, found : qfoundchar substr }
        CharRule (Not chx) -> NegCharacterError { notExpected : G.expected chx, found : qfoundchar substr }
        CharRule (Range from to) -> CharacterRangeError { from : G.expected from, to : G.expected to, found : qfoundchar substr }
        CharRule Any -> AnyCharacterError { found : qfoundchar substr }
        Choice _ -> ChoiceError { }
        Sequence _ -> SequenceError { index : 0 }  -- FIXME
        Ref _ name -> RuleNotFoundError { name }
        RepSep _ _ -> RepSepError { occurence : 0 } -- FIXME
        Placeholder -> PlaceholderError
        None -> Unknown
    where
        qfoundstr expected =
            if String.length substr > 0 then G.found $ String.take (String.length expected) substr else G.eoi
        qfoundchar =
            String.take 1 >>> SCU.charAt 0 >>> maybe G.eoi G.found -- FIXME: EOL/EOF
