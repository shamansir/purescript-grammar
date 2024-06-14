module AST.Parser where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Map (empty) as Map
import Data.String (length, take) as String
import Data.String.CodeUnits (charAt, length, splitAt) as SCU

import Grammar (Grammar, Rule(..), AST(..), Tree(..), Attempt(..), ASTNode, RuleSet, At(..), Error(..), CharRule(..), Found(..), Expected(..))
import Grammar (set, main, found, eoi, expected) as G


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


_text :: forall a. (Rule -> a) -> Rule -> String -> P a
_text f rule expected = Parser \{ position, next } ->
    let
        length = SCU.length expected
        { before, after } = SCU.splitAt length next
    in
        if before == expected then
            _lmatch f rule after position $ position + length
        else
            _lfail rule next position $ TextError { expected : G.expected expected, found : if String.length next > 0 then G.found before else G.eoi }


_lmatch :: forall a. (Rule -> a) -> Rule -> String -> Int -> Int -> Step a Unit
_lmatch f rule nextStr curPos nextPos =
    { value: unit
    , rest: { next: nextStr, position: nextPos }
    , node :
        Leaf
            { rule
            , result : Match { start : curPos, end : nextPos } $ f rule
            }
    }


_lfail :: forall a. Rule -> String -> Int -> Error -> Step a Unit
_lfail rule next position error =
    { value: unit
    , rest: { position, next }
    , node :
        Leaf
            { rule
            , result : Fail position error
            }
    }


text :: forall a. a -> String -> P a
text a expected = _text (const a) (Text expected) expected



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
