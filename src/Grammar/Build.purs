module Grammar.Build where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))

-- import Record.Extra

import Grammar (Grammar(..), Rule(..), WhichChar(..), RuleName, CaptureName)



infixr 6 type Tuple as :-
infixr 6 Tuple as :-


-- TODO: grammar :: forall rules. Rule -> Record rules -> Grammar
grammar :: Rule -> Array (RuleName :- Rule) -> Grammar
grammar main = Grammar main <<< Map.fromFoldable


sequence :: Array Rule -> Rule
sequence = Sequence


choice :: Array Rule -> Rule
choice = Choice


ref :: RuleName -> Rule -- TODO: Symbol
ref = Ref Nothing


refAs :: CaptureName -> RuleName -> Rule -- TODO: Symbol
refAs = Ref <<< Just


text :: String -> Rule
text = Text


repSep :: { rep :: Rule, sep :: Rule } -> Rule -- TODO: some operator for it
repSep { rep, sep } = RepSep rep sep


anyChar :: Rule
anyChar = Char Any


char :: Char -> Rule
char = Char <<< Single


notChar :: Char -> Rule
notChar = Char <<< Not


range :: { from :: Char, to :: Char } -> Rule
range { from, to } = Char $ Range from to


placeholder :: Rule
placeholder = Placeholder