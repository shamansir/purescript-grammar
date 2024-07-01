module Grammar.AST.Point where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.String.CodeUnits (toCharArray, fromCharArray) as String
import Data.Array (index, snoc) as Array
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (fold, foldl)

import Grammar (RuleName) as G


-- TODO  : could be Point a = Tree (At /\ a)
--         but for choice and rule, we have to have a single element stored

data Point a
    = Rule a G.RuleName (Point a)
    | OneOf a Int (Point a)
    | Many a (Array (Point a))
    -- | Reps a (Array (Point a))
    | Value a


instance Show a => Show (Point a) where
    show = case _ of
        Rule a name point -> "( " <> show a <> " | " <> name <> " : " <> show point <> " )"
        OneOf a idx point -> "< " <> show a <> " | " <> show idx <> " : " <> show point <> " >"
        Many a points -> "[ " <> show a <> " | " <> String.joinWith " , " (show <$> points ) <> " ]"
        Value a -> "{ " <> show a <> " }"


infixr 7 ifChosenFlipped as ?#
infixr 7 ifRuleFlipped as ?~
infixr 7 valueIfRuleFlipped as ?.
infixr 7 takeFlipped as #^
infixr 7 pointChosen as #>


infixr 7 ifChosen as #?
infixr 7 ifRule as ~?
infixr 7 valueIfRule as .?
infixr 7 take as ^#
infixr 7 pointChosenFlipped as <#

-- infixr 8 _cellAt as @^
-- infixr 8 _nodeAt as @~


ifChosenFlipped    = flip ifChosen
ifRuleFlipped      = flip ifRule
valueIfRuleFlipped = flip valueIfRule
takeFlipped        = flip take
pointChosenFlipped = flip pointChosen


-- type InPoint a = Maybe (Point a)


ifChosen :: forall a. Int -> Point a -> Maybe (Point a)
ifChosen n (OneOf _ idx oopoint) | idx == n  = Just oopoint
ifChosen _ _                     | otherwise = Nothing


ifRule :: forall a. String -> Point a -> Maybe (Point a)
ifRule name (Rule _ fname rpoint) | fname == name = Just rpoint
ifRule _    _                     | otherwise     = Nothing


valueIfRule :: forall a. String -> Point a -> Maybe a
valueIfRule name (Rule v fname _) | fname == name = Just v
valueIfRule _    _                | otherwise     = Nothing


take :: forall a. Int -> Point a -> Maybe (Point a)
take n (Many _ points) = Array.index points n
take _ _                = Nothing


points :: forall a. Point a -> Maybe (Array (Point a))
points (Many _ points) = Just points
points _                = Nothing


pointChosen :: forall a b. (Int -> Point a -> Maybe b) -> Point a -> Maybe b
pointChosen f (OneOf a n oopoint) = f n oopoint
pointChosen _ _                   = Nothing


collectText :: Point String -> String
collectText = case _ of
    Rule chunk _ _ -> chunk
    OneOf chunk _ _ -> chunk
    Many chunk _ -> chunk
    Value chunk -> chunk


collectContent :: Array (Point String) -> String
collectContent = _unescape <<< fold <<< map collectText


_unescape :: String -> String
_unescape = String.toCharArray >>> foldl _foldPairs ([] /\ false) >>> Tuple.fst >>> String.fromCharArray

-- FIXME: it is still unclear why we need to unescape:
--          yes, the `text` (and so `stringChar`) rule collects chars '\\' and everything after as separate characters
--          so when we combine them using fold/monoid, we get double-escaping, but may be we could fix it somehow
--          if we do it properly;
-- FIXME: may be it is faster to do it in JavaScript or there's some utility function in String.Extra or somewhere
_foldPairs :: Array Char /\ Boolean -> Char -> Array Char /\ Boolean
_foldPairs (arr /\ false) '\\' = arr /\ true
_foldPairs (arr /\ true)  '\\' = Array.snoc arr '\\' /\ false
_foldPairs (arr /\ true)  ch   = (Array.snoc arr $ _escaped ch) /\ false
_foldPairs (arr /\ false) ch   = Array.snoc arr ch /\ false


_escaped :: Char -> Char
_escaped = case _ of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    '\\' -> '\\'
    'x' -> '\x'
    '\'' -> '\''
    ch -> ch
