module Grammar where

import Prelude

import Data.Map (Map)
import Data.Map (empty) as Map


type RuleName = String


data Grammar = Grammar (Map RuleName Rule)


data Rule
    = Ref RuleName
    | Range Char Char
    | Sequence (Array Grammar)
    | Switch (Array Grammar)
    | Text String
    | Char Char
    | Not Grammar
    | RepSep Grammar Grammar



type Position = { start :: Int, end :: Int }


data AST a
    = Nil
    | Node a (Array (AST a))
    | Leaf RuleName Rule Position a



data Failure = Failure Position



empty :: Grammar
empty = Grammar Map.empty


parse :: forall a. String -> (Rule -> a) -> Grammar -> AST a
parse _ _ _ = Nil