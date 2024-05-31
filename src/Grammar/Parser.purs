module Grammar.Parser where

import Prelude

import Data.Tuple (uncurry, fst) as Tuple
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
import Parsing (fail) as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String as P
import Parsing.String.Basic as P


type P a = Parser String a


parser :: P Grammar
parser =
    map
        (NEA.toArray >>> Array.catMaybes >>> Map.fromFoldable >>> makeGrammar)
        <$> PA.many1
            $ P.choice [ Just <$> ruleLine, Nothing <$ emptyLine, Nothing <$ commentLine ]
    where
        makeGrammar map =
            Grammar.from
                (Map.lookup "main" map # fromMaybe Grammar.Placeholder)
                $ Map.delete "main" map


emptyLine :: P Unit
emptyLine =
    ws <* eol


commentLine :: P Unit
commentLine =
    void $ P.manyTill_ P.anyChar eol


ruleLine :: P (Grammar.RuleName /\ Grammar.Rule)
ruleLine = do --defer \_ -> do
    ruleName <- PA.many1 P.alphaNum
    ws
    _ <- P.string ":-"
    ws
    theRule <- rule
    ws
    _ <- P.char '.'
    ws
    P.optional (P.try eol)
    pure $ toString ruleName /\ theRule


rule :: P Grammar.Rule
rule = defer \_ ->
    P.choice
        -- FIXME: using P.try in choice is considered bad practice
        [ P.try anyChar <?> "any char rule"
        , P.try seq <?> "sequence rule"
        , P.try choice <?> "choice rule"
        , P.try text <?> "text rule"
        , P.try charRange <?> "char range rule"
        , P.try notChar <?> "not-char rule"
        , P.try char <?> "char rule"
        , P.try repSep <?> "rep-sep rule"
        , P.try repSepAlt <?> "rep-sep-alt rule"
        , P.try ref <?> "ref rule"
        , P.try placeholder <?> "placeholder rule"
        ]
        -- [ anyChar
        -- , seq
        -- , choice
        -- , text
        -- , charRange
        -- , notChar
        -- , char
        -- , ref
        -- , repSep
        -- , repSepAlt
        -- , placeholder
        -- ]


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


choice :: P Grammar.Rule
choice = defer \_ ->
    Grammar.Choice <$>
    NEL.toUnfoldable <$>
    P.between
        (P.char '(')
        (P.char ')')
        (P.sepBy1 (ws *> rule <* ws) $ P.char '|')


ref :: P Grammar.Rule
ref = defer \_ -> do
    mbCapture <- P.optionMaybe $ P.try $ do
        captureName <- _ident
        _ <- P.char ':'
        pure captureName
    ruleName <- _ident
    pure $ Grammar.Ref mbCapture ruleName


text :: P Grammar.Rule
text = defer \_ ->
    Grammar.Text <$>
    String.fromCharArray <$>
    NEA.toUnfoldable <$>
    P.between
        (P.char '"')
        (P.char '"')
        (PA.many1 $ _notBut (P.char '"') P.anyChar)


char :: P Grammar.Rule
char = defer \_ ->
    Grammar.CharRule <$>
    Grammar.Single <$>
    _char


notChar :: P Grammar.Rule
notChar = defer \_ ->
    Grammar.CharRule <$>
    Grammar.Single <$> do
    (P.char '^' *> _char)


charRange :: P Grammar.Rule
charRange = defer \_ ->
    Grammar.CharRule <$>
    Tuple.uncurry Grammar.Range <$>
    P.between
        (P.char '[')
        (P.char ']')
        (do
            from <- P.alphaNum
            void $ P.char '-'
            to <- P.alphaNum
            pure $ from /\ to
        )


placeholder :: P Grammar.Rule
placeholder = defer \_ ->
    pure Grammar.Placeholder <* P.string "???"


repSep :: P Grammar.Rule
repSep = defer \_ ->
    Tuple.uncurry Grammar.RepSep <$> do
        void $ P.string "repSep"
        P.between
            (P.char '(')
            (P.char ')')
            (do
                ws
                rep <- rule
                ws
                void $ P.char ','
                ws
                sep <- rule
                ws
                pure $ rep /\ sep
            )


repSepAlt :: P Grammar.Rule
repSepAlt = defer \_ ->
    Tuple.uncurry Grammar.RepSep <$> do
        void $ P.string "+" -- eliminates left recursion
        rep <- rule
        ws
        void $ P.string "//"
        ws
        sep <- rule
        ws
        pure $ rep /\ sep


_ident :: P String
_ident =
    String.fromCharArray
    <$> NEA.toArray
    <$> PA.many1 P.alphaNum


_char :: P Char
_char =
    P.between
        (P.char '\'')
        (P.char '\'')
        (P.anyChar)


_notBut :: forall a x. Show x => P x -> P a -> P a
_notBut exclude pass = do
    mbExclude <- P.optionMaybe $ P.try exclude
    case mbExclude of
        Just x -> P.fail $ "found " <> show x
        Nothing -> pass


ws :: P Unit
ws = P.skipSpaces


ws1 :: P Unit
ws1 =
    void $ PA.many1 P.space


eol :: P Unit
eol = void $ P.char '\n'


toString :: NonEmptyArray Char -> String
toString = NEA.toArray >>> String.fromCharArray
