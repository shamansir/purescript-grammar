module Grammar.AST.Chunk where

import Prelude

import Data.String (Pattern(..))
import Data.String (split, length) as String
import Data.String.CodeUnits (slice) as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (foldl) as Array
import Data.Newtype (class Newtype, wrap, unwrap)


type Range = { start :: Int, end :: Int }


type Position = Int


type LineColumn = { line :: Int, column :: Int }


newtype Chunk =
    Chunk
        { from :: LineColumn
        , to :: LineColumn
        , content :: String
        }


derive instance Newtype Chunk _


type PosFinder =
    Position -> LineColumn -- TODO: Maybe LineColumn


posFinderFor :: String -> PosFinder
posFinderFor source =
    findPos
    where
        sourceLen :: Int
        sourceLen = String.length source
        breaks :: Array Int
        breaks = source # String.split (Pattern "\n") <#> String.length
         -- we could optimize / memoize it knowing that range end is always after the start or equal to it
        findPos :: Int -> { line :: Int, column :: Int }
        findPos n = breaks # Array.foldl (foldF n) (0 /\ { line : 0, column : 0 }) # Tuple.snd
        foldF :: Int -> Int /\ { line :: Int, column :: Int } -> Int ->  Int /\ { line :: Int, column :: Int }
        foldF n (curLineStart /\ { line, column }) curLineLength =
            let
                nextLineStart = min sourceLen $ curLineStart + curLineLength + 1 -- add 1 for the '\n' symbol in the end
                left = n - curLineStart
            in if left >= 0 && left < (curLineLength + 1) then -- add 1 for the '\n' symbol in the end
                nextLineStart /\
                    { line : line
                    , column : min left curLineLength
                    }
            else if left >= 0 && left > curLineLength then
                nextLineStart /\ { line : line + 1, column : 0 }
            else
                nextLineStart /\ { line, column } -- found


for :: String -> PosFinder -> Range -> Chunk
for str findPos rng =
    Chunk
        { from : findPos rng.start
        , to : findPos rng.end
        , content : String.slice rng.start rng.end str
        }


content :: Chunk -> String
content = unwrap >>> _.content


from :: Chunk -> LineColumn
from = unwrap >>> _.from


to :: Chunk -> LineColumn
to = unwrap >>> _.to