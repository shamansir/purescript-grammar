module Grammar.AST.Chunk where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (slice) as String

import Grammar.AST.Location (Range)


newtype Chunk =
    Chunk
        { content :: String }


derive instance Newtype Chunk _


content :: Chunk -> String
content = unwrap >>> _.content


from :: String -> Range -> Chunk
from src rng = Chunk $ { content : String.slice rng.start rng.end src }