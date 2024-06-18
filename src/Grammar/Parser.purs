module Grammar.Parser where

import Prelude

import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodeUnits (fromCharArray) as String
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Control.Lazy (defer)

import Grammar (Grammar, CharX(..), toChar)
import Grammar as Grammar

import Parsing (Parser)
import Parsing (fail) as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String (anyChar, char, string) as P
import Parsing.String.Basic (alphaNum, skipSpaces, space) as P


type P a = Parser String a


parser :: P Grammar
parser =
    map
        (NEA.toArray >>> Array.catMaybes >>> Map.fromFoldable >>> makeGrammar)
        <$> PA.many1
            $ P.choice [ Nothing <$ emptyLine, Nothing <$ commentLine, Just <$> ruleLine ]
        <?> "expected at least one rule"
    where
        makeGrammar map =
            Grammar.from
                (Map.lookup "main" map # fromMaybe Grammar.Placeholder)
                $ Map.delete "main" map


emptyLine :: P Unit
emptyLine =
    ws <* eol <?> "expected empty line"


commentLine :: P Unit
commentLine =
    void $ P.char '#' *> P.manyTill_ P.anyChar eol <?> "expected comment"


ruleLine :: P (Grammar.RuleName /\ Grammar.Rule)
ruleLine = do --defer \_ -> do
    ruleName <- _ident
    ws
    _ <- P.string ":-"
    ws
    theRule <- rule <?> "expected rule"
    ws
    _ <- P.char '.'
    ws
    P.optional (P.try eol)
    pure $ ruleName /\ theRule
    <?> "expected rule line"


rule :: P Grammar.Rule
rule = defer \_ ->
    P.choice
        [ anyChar <?> "any char rule"
        , seq <?> "sequence rule"
        , choice <?> "choice rule"
        , text <?> "text rule"
        , charRange <?> "char range rule"
        , notChar <?> "not-char rule"
        , char <?> "char rule"
        , repSep <?> "rep-sep rule"
        , repSepAlt <?> "rep-sep-alt rule"
        , ref <?> "ref rule"
        , placeholder <?> "placeholder rule"
        ] <?> "one of the rules"


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
    _buildString <$>
    P.between
        (P.char '"')
        (P.char '"')
        (PA.many $ _textChar '"' '\\')


char :: P Grammar.Rule
char = defer \_ ->
    Grammar.CharRule <$>
    Grammar.Single <$>
    _char


notChar :: P Grammar.Rule
notChar = defer \_ ->
    Grammar.CharRule <$>
    Grammar.Not <$> do
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
        P.between
            (P.char '/')
            (P.char '/')
            (do
                ws
                rep <- rule
                void $ P.char '+'
                ws
                void $ P.string "//"
                ws
                sep <- rule
                ws
                pure $ rep /\ sep
            )


_ident :: P String
_ident =
    String.fromCharArray
    <$> NEA.toArray
    <$> (PA.many1 P.alphaNum <?> "expected identifier")


_char :: P CharX
_char =
    P.between
        (P.char '\'')
        (P.char '\'')
        (_textChar ''' '\\')


_loadChar :: CharX -> Char
_loadChar = case _ of
    Escaped ch -> ch
    Raw ch -> ch


_buildString :: Array CharX -> String
_buildString = map toChar >>> String.fromCharArray


_textChar :: Char -> Char -> P CharX
_textChar terminate escape = do
    x <- P.lookAhead P.anyChar
    if x == terminate then
        P.fail $ "found " <> show x
    else if x == escape then
        P.anyChar *> P.anyChar <#> Escaped
    else
        P.anyChar <#> Raw


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
    void $ PA.many1 P.space <?> "expected whitespace"


eol :: P Unit
eol = void $ P.char '\n' <?> "expected EOL"


toString :: NonEmptyArray Char -> String
toString = NEA.toArray >>> String.fromCharArray
