module Grammar.Self.Extract where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (index, catMaybes, find, mapWithIndex, foldr, foldl, snoc) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (fromFoldable, lookup, delete) as Map
import Data.Foldable (fold)
import Data.String (joinWith) as String
import Data.String.CodeUnits (charAt, toCharArray, fromCharArray) as String

import Grammar (Grammar(..))
import Grammar (Rule, empty) as Grammar
import Grammar (Rule(..), RuleName, WhichChar(..), fromChar, fromString) as G
import Grammar.AST (AST(..), At, ASTNode)
import Grammar.AST (root, Cell, Attempt(..), Range) as AST

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (children, value) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (traverse) as Tree

-- import Debug as Debug


{- data GrammarItem
    = GRuleDef String Grammar.Rule
    | GRule Grammar.Rule -}

data Match a
    = Rule a G.RuleName (Match a)
    | OneOf a Int (Match a)
    | Many a (Array (Match a))
    -- | Reps a (Array (Match a))
    | Value a


instance Show a => Show (Match a) where
    show = case _ of
        Rule a name match -> "( " <> show a <> " | " <> name <> " : " <> show match <> " )"
        OneOf a idx match -> "< " <> show a <> " | " <> show idx <> " : " <> show match <> " >"
        Many a matches -> "[ " <> show a <> " | " <> String.joinWith " , " (show <$> matches ) <> " ]"
        Value a -> "{ " <> show a <> " }"


{- TODO FIXME
data Match a
    = Rule G.RuleName a (Match a)
    | Many a (Array (Match a))
    | One a (Match a)
-}


_isMatch :: forall a. ASTNode a -> Boolean
_isMatch node = case _.result $ Tree.value node of
    AST.Match _ _ -> true
    AST.Fail _ _ -> false


convert :: forall a. ASTNode a -> Maybe (Match a)
convert node =
    case _.result $ Tree.value node of
        AST.Match _ a ->
            case _.rule $ Tree.value node  of
                G.Sequence _ -> Just $ Many a $ Array.catMaybes $ (convert <$> Tree.children node)
                G.Choice _ ->
                    Tree.children node
                        # mapWithIndex (/\)
                        # Array.find (Tuple.snd >>> _isMatch)
                        <#> map convert
                        >>= \(n /\ mbMatch) -> mbMatch <#> OneOf a n
                G.RepSep _ _ ->
                    Just
                        $ Many a
                        $ Array.catMaybes
                        $ Array.mapWithIndex
                            (\idx mbMatch -> if idx `mod` 2 == 1 then Nothing else mbMatch)
                            (convert <$> Tree.children node)
                G.Ref mbCapture ruleName ->
                    Array.index (Tree.children node) 0
                        >>= convert
                        <#> Rule a (fromMaybe ruleName mbCapture)
                G.Text _ -> Just $ Value a
                G.Char _ -> Just $ Value a
                G.Placeholder -> Nothing
                G.None -> Nothing
        AST.Fail _ _ -> Nothing


extract :: AST String -> Grammar
extract =
    -- let
    --     _ = Debug.spy "ast" $ show ast
    --     _ = Debug.spy "convert" $ show $ convert $ AST.root ast
    -- in ast # AST.root # convert # flip bind load # fromMaybe Grammar.empty
    AST.root >>> convert >>> flip bind load >>> fromMaybe Grammar.empty


_ifChosen :: forall a. Int -> Match a -> Maybe (Match a)
_ifChosen n (OneOf _ idx oomatch) | idx == n  = Just oomatch
_ifChosen _ _                     | otherwise = Nothing


_ifRule :: forall a. String -> Match a -> Maybe (Match a)
_ifRule name (Rule _ fname rmatch) | fname == name = Just rmatch
_ifRule _    _                     | otherwise     = Nothing


_valueIfRule :: forall a. String -> Match a -> Maybe a
_valueIfRule name (Rule v fname _) | fname == name = Just v
_valueIfRule _    _                | otherwise     = Nothing


_take :: forall a. Int -> Match a -> Maybe (Match a)
_take n (Many _ matches) = Array.index matches n
_take _ _                = Nothing


_matches :: forall a. Match a -> Maybe (Array (Match a))
_matches (Many _ matches) = Just matches
_matches _                = Nothing


_matchChosen :: forall a b. (Int -> Match a -> Maybe b) -> Match a -> Maybe b
_matchChosen f (OneOf a n oomatch) = f n oomatch
_matchChosen _ _                   = Nothing


load :: Match String -> Maybe Grammar
load (Many _ ruleDefnMatches) =
    loadGrammar $ Array.catMaybes $ flip bind extractRuleDef <$> _ifChosen 0 <$> ruleDefnMatches

    where

        loadGrammar :: Array { ruleName :: G.RuleName, rule :: Grammar.Rule } -> Maybe Grammar
        loadGrammar ruleDefs =
            let allRules = Map.fromFoldable $ (\{ ruleName, rule } -> ruleName /\ rule ) <$> ruleDefs
            in
                Map.lookup "main" allRules <#> \mainRule -> Grammar mainRule $ Map.delete "main" allRules

        extractRuleDef :: Match String -> Maybe { ruleName :: G.RuleName, rule :: Grammar.Rule }
        extractRuleDef match = do
            ruleDefn <- _ifRule "ruleDefn" match
            ruleName <- _take 0 ruleDefn >>= _valueIfRule "ident"
            rule     <- _take 4 ruleDefn >>= extractRule "rule"
            pure { ruleName, rule }


        extractRule :: String -> Match String -> Maybe Grammar.Rule
        extractRule expectedName match =
            _ifRule expectedName match
                >>= _matchChosen \n oomatch ->
                case n of
                    0 -> seq oomatch
                    1 -> choice oomatch
                    2 -> charRule' oomatch
                    3 -> text oomatch
                    4 -> repSep oomatch
                    5 -> oomatch # _ifRule "placeholder" $> pure G.Placeholder
                    6 -> ref oomatch
                    _ -> Nothing


        ref :: Match String -> Maybe G.Rule
        ref oomatch = do
            refMatch <- _ifRule "ref" oomatch
            let mbCaptureName = _take 0 refMatch >>= _ifChosen 0 >>= _take 0 <#> collectText
            ruleName <- _take 1 refMatch <#> collectText
            pure $ G.Ref mbCaptureName ruleName


        repSep :: Match String -> Maybe G.Rule
        repSep oomatch = do
            repSepMatch <- _ifRule "repSep" oomatch
            rep <- _take 2 repSepMatch >>= extractRule "rep"
            sep <- _take 4 repSepMatch >>= extractRule "sep"
            pure $ G.RepSep rep sep


        seq :: Match String -> Maybe G.Rule
        seq oomatch = do
            seqMatch <- _ifRule "seq" oomatch
            ruleMatches <- _take 2 seqMatch >>= _matches
            pure $ G.Sequence $ Array.catMaybes $ extractRule "rule" <$> ruleMatches


        choice :: Match String -> Maybe G.Rule
        choice oomatch = do
            chMatch <- _ifRule "choice" oomatch
            ruleMatches <- _take 2 chMatch >>= _matches
            pure $ G.Choice $ Array.catMaybes $ extractRule "rule" <$> ruleMatches


        text :: Match String -> Maybe G.Rule
        text oomatch = do
            tMatch <- _ifRule "text" oomatch
            strCharMatches <- _take 1 tMatch >>= _matches
            pure $ G.Text $ collectContent strCharMatches


        charRule' :: Match String -> Maybe G.Rule
        charRule' oomatch =
            _ifRule "charRule" oomatch
                >>= _matchChosen \n ioomatch ->
                case n of
                    0 -> do -- char range
                        crMatch  <- _ifRule "charRange" ioomatch
                        fromChar <- _take 1 crMatch <#> collectText >>= String.charAt 0
                        toChar   <- _take 3 crMatch <#> collectText >>= String.charAt 0
                        pure $ G.Char $ G.Range fromChar toChar
                    1 -> do -- not char
                        ncMatch  <- _ifRule "notChar" ioomatch
                        charStr <- _take 1 ncMatch <#> collectText <#> unescape
                        charx <- charStr # String.charAt 1 <#> G.fromChar
                        pure $ G.Char $ G.Not charx
                    2 -> do -- single char
                        scMatch  <- _ifRule "singleChar" ioomatch
                        charStr <- _take 1 scMatch <#> collectText
                        charx <- charStr # G.fromString
                        pure $ G.Char $ G.Single charx
                    3 -> do
                        _ <- _ifRule "anyChar" ioomatch
                        pure $ G.Char G.Any
                    _ -> Nothing


        collectText :: Match String -> String
        collectText = case _ of
            Rule chunk _ _ -> chunk
            OneOf chunk _ _ -> chunk
            Many chunk _ -> chunk
            Value chunk -> chunk

        collectContent :: Array (Match String) -> String
        collectContent = unescape <<< fold <<< map collectText

        unescape :: String -> String
        unescape = String.toCharArray >>> Array.foldl foldPairs ([] /\ false) >>> Tuple.fst >>> String.fromCharArray

        -- FIXME: it is still unclear why we need to unescape:
        --          yes, the `text` (and so `stringChar`) rule collects chars '\\' and everything after as separate characters
        --          so when we combine them using fold/monoid, we get double-escaping, but may be we could fix it somehow
        --          if we do it properly;
        -- FIXME: may be it is faster to do it in JavaScript or there's some utility function in String.Extra or somewhere
        foldPairs :: Array Char  /\ Boolean -> Char -> Array Char /\ Boolean
        foldPairs (arr /\ false) '\\' = arr /\ true
        foldPairs (arr /\ true)  '\\' = Array.snoc arr '\\' /\ false
        foldPairs (arr /\ true)  ch   = (Array.snoc arr $ escaped ch) /\ false
        foldPairs (arr /\ false) ch   = Array.snoc arr ch /\ false

        escaped :: Char -> Char
        escaped = case _ of
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            '\\' -> '\\'
            'x' -> '\x'
            '\'' -> '\''
            ch -> ch

load _ = Nothing



{-
extract :: forall a. AST a -> Tree Match
extract ast = Tree.traverse traverseF $ AST.root ast


infixr 7 _ruleAt as @#
infixr 8 _cellAt as @^
infixr 8 _nodeAt as @~


_ruleAt :: forall a. ASTNode a -> Int -> Maybe Grammar.Rule
_ruleAt = map (map _.rule) <<< _cellAt


_nodeAt :: forall a. ASTNode a -> Int -> Maybe (ASTNode a)
_nodeAt node idx =
    Array.index (Tree.children node) idx


_cellAt :: forall a. ASTNode a -> Int -> Maybe (AST.Cell a)
_cellAt =
    map (map Tree.value) <<< _nodeAt
    -}


{-
_matched :: G.RuleName -> Grammar.Rule -> Boolean
_matched ruleName rule =
    case rule of
        G.Ref (Just cn) _ -> cn == ruleName
        -- G.Ref (Just _) rn -> rn == ruleName
        G.Ref Nothing rn -> rn == ruleName
        _ -> false


_matchedMb :: G.RuleName -> Maybe Grammar.Rule -> Boolean
_matchedMb ruleName rule =
-}


-- _load :: G.RuleName -> AST.Cell a -> Maybe (AST.Cell a)
-- _load ruleName { rule, result } =
--     case


{-
traverseF :: forall a. Path -> AST.Cell a -> ASTNode a -> Match
traverseF _ { rule, result } node =
    case rule of
        G.Ref Nothing "ruleDefn" ->
            case node @# 0 /\ node @# 4 of
                Just (G.Ref Nothing "ident") /\ Just (G.Ref Nothing "rule") ->
                    let
                        mbIdent = _contentOf <$> node @^ 0
                        mbRule = loadRule =<< (node @~ 4)
                    in
                        fromMaybe Skip $ RuleDef <$> mbIdent <*> mbRule
                _ -> Skip
        _ -> Skip


_contentOf :: forall a. AST.Cell a -> String
_contentOf { result } =
    case result of
        AST.Match { start, end } _ -> "" -- FIXME
        AST.Fail _ _ -> ""


loadRule :: forall a. ASTNode a -> Maybe Grammar.Rule
loadRule node =
    case _.rule $ Tree.value node of
        Choice _ ->
-}