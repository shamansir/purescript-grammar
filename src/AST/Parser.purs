module AST.Parser where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Map (empty) as Map
import Data.String (length, take) as String
import Data.String.CodeUnits (charAt, length, splitAt, drop) as SCU
-- import Data.String.CodePoints (head) as SCP

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
runParser (Parser p) str =
    AST $ _.node $ p { position : 0, next : str }


parseRule :: forall a. RuleSet -> (Rule -> a) -> At -> Rule -> P a
parseRule set f at rule =
    case rule of
        Text expected -> _text f rule expected
        _ -> Parser $ \state -> { value : unit, rest : state, node : Leaf { rule : Ref Nothing "main", result : Match { start : 0, end : 0 } $ f (Ref Nothing "main") } }


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


_makeError :: String -> Rule -> Error
_makeError substr =
    case _ of
        Text expected -> TextError { expected : G.expected expected, found : qfoundstr expected }
        CharRule (Single chx) -> CharacterError { expected : G.expected chx, found : qfoundchar substr }
        CharRule (Not chx) -> NegCharacterError { notExpected : G.expected chx, found : qfoundchar substr }
        CharRule (Range from to) -> CharacterRangeError { from : G.expected from, to : G.expected to, found : qfoundchar substr }
        CharRule Any -> AnyCharacterError { found : qfoundchar substr }
        Choice _ -> ChoiceError { }
        Sequence _ -> SequenceError { }
        Ref _ name -> RuleNotFoundError { name }
        RepSep _ _ -> RepSepError { occurence : 0 } -- FIXME
        Placeholder -> PlaceholderError
        None -> Unknown
    where
        qfoundstr expected =
            if String.length substr > 0 then G.found $ String.take (String.length expected) substr else G.eoi
        qfoundchar =
            String.take 1 >>> SCU.charAt 0 >>> maybe G.eoi G.found -- FIXME: EOL/EOF
