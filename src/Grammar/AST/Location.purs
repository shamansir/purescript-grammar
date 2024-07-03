module Grammar.AST.Location where

import Prelude


type Range = { start :: Int, end :: Int }


type Position = Int


type LineColumn = { line :: Int, column :: Int }


type PosFinder =
    Position -> LineColumn -- TODO: Maybe LineColumn