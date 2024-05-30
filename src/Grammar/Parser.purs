module Grammar.Parser where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodeUnits as String
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Control.Lazy (defer)

import Grammar (Grammar)
import Grammar as Grammar

import Parsing (Parser)
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String as P
import Parsing.String.Basic as P


type P a = Parser String a


parser :: P Grammar
parser =
    map
        (Array.catMaybes >>> Map.fromFoldable >>> makeGrammar)
        <$> PA.many
            $ P.choice [ Just <$> ruleLine, Nothing <$ emptyLine ]
    where
        makeGrammar map =
            Grammar.from
                (Map.lookup "main" map # fromMaybe Grammar.Placeholder)
                $ Map.delete "main" map


emptyLine :: P Unit
emptyLine =
    ws <* eol


ws :: P Unit
ws = P.skipSpaces


ws1 :: P Unit
ws1 =
    void $ PA.many1 P.space


ruleLine :: P (Grammar.RuleName /\ Grammar.Rule)
ruleLine = do --defer \_ -> do
    ruleName <- PA.many1 P.alphaNum
    ws
    _ <- P.string ":-"
    ws
    theRule <- rule
    ws
    _ <- P.char '.'
    pure $ toString ruleName /\ theRule


rule :: P Grammar.Rule
rule = defer \_ ->
    P.choice
        [ anyChar
        , seq
        ]


anyChar :: P Grammar.Rule
anyChar = defer \_ ->
    Grammar.CharRule Grammar.Any <$ P.char '.'


seq :: P Grammar.Rule
seq = defer \_ ->
    Grammar.Sequence <$>
    NEL.toUnfoldable <$>
    P.between
        (P.char '[')
        (P.char ']')
        (P.sepBy1 (ws *> rule <* ws) $ P.char ',')


eol :: P Unit
eol = void $ P.char '\n'


toString :: NonEmptyArray Char -> String
toString = NEA.toArray >>> String.fromCharArray
