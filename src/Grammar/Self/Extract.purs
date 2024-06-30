module Grammar.Self.Extract where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (index, catMaybes, find, mapWithIndex) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (fromFoldable, lookup) as Map
import Data.Foldable (fold)
import Data.String.CodeUnits (charAt) as String

import Grammar (Grammar(..))
import Grammar (Rule, empty) as Grammar
import Grammar (Rule(..), RuleName, WhichChar(..), fromChar) as G
import Grammar.AST (AST(..), At, ASTNode)
import Grammar.AST (root, Cell, Attempt(..), Range) as AST

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (children, value) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (traverse) as Tree


{- data GrammarItem
    = GRuleDef String Grammar.Rule
    | GRule Grammar.Rule -}

data Match a
    = Rule a G.RuleName (Match a)
    | OneOf a Int (Match a)
    | Many a (Array (Match a))
    -- | Reps a (Array (Match a))
    | Value a


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
extract = AST.root >>> convert >>> flip bind load >>> fromMaybe Grammar.empty


load :: Match String -> Maybe Grammar
load (Rule _ "main" mainMatch) =
    case mainMatch of
        Many _ ruleMatches ->
            loadGrammar $ Array.catMaybes $ extractRuleDef <$> ruleMatches
        _ -> Nothing

    where

        loadGrammar :: Array { ruleName :: G.RuleName, rule :: Grammar.Rule } -> Maybe Grammar
        loadGrammar ruleDefs =
            let allRules = Map.fromFoldable $ (\{ ruleName, rule } -> ruleName /\ rule ) <$> ruleDefs
            in
                Map.lookup "main" allRules <#> \mainRule -> Grammar mainRule allRules

        extractRuleDef :: Match String -> Maybe { ruleName :: G.RuleName, rule :: Grammar.Rule }
        extractRuleDef = case _ of
            Rule _ "ruleDefn" match ->
                case match of
                    Many _ matches ->
                        let
                            ruleName =
                                case Array.index matches 0 of
                                    Just (Rule text "ident" _) -> text
                                    _ -> ""
                            mbRule =
                                Array.index matches 4 >>= extractRule
                        in
                            mbRule <#> \rule -> { ruleName, rule }
                    _ -> Nothing
            _ -> Nothing

        extractRule :: Match String -> Maybe Grammar.Rule
        extractRule = case _ of
            Rule _ "rule" rmatch ->
                case rmatch of
                    OneOf _ 0 oomatch -> -- seq
                        case oomatch of
                            Rule _ "seq" seqMatch ->
                                case seqMatch of
                                    Many _ sequence ->
                                        case Array.index sequence 2 of
                                            Just (Many _ rules) ->
                                                Just $ G.Sequence $ Array.catMaybes $ extractRule <$> rules
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                    OneOf _ 1 oomatch -> -- choice
                        case oomatch of
                            Rule _ "choice" chMatch ->
                                case chMatch of
                                    Many _ sequence ->
                                        case Array.index sequence 2 of
                                            Just (Many _ rules) ->
                                                Just $ G.Choice $ Array.catMaybes $ extractRule <$> rules
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                    OneOf _ 2 oomatch -> -- charRule
                        case oomatch of
                            Rule _ "charRule" chruleMatch ->
                                case chruleMatch of
                                    OneOf _ 0 ioomatch -> -- char range
                                        case ioomatch of
                                            Many _ sequence ->
                                                let
                                                    mbFromChar = Array.index sequence 1 <#> collectText >>= String.charAt 0 -- a.k.a "to"
                                                    mbToChar = Array.index sequence 3 <#> collectText >>= String.charAt 0  -- a.k.a "from"
                                                in
                                                    G.Char <$> (G.Range <$> mbFromChar <*> mbToChar)
                                            _ -> Nothing
                                    OneOf _ 1 ioomatch -> -- not char
                                        case ioomatch of
                                            Many _ sequence ->
                                                let charV = Array.index sequence 1 <#> collectText
                                                in charV >>= String.charAt 0 <#> G.fromChar <#> G.Not <#> G.Char
                                            _ -> Nothing
                                    OneOf _ 2 ioomatch -> -- single char
                                        case ioomatch of
                                            Many _ sequence ->
                                                let charV = Array.index sequence 1 <#> collectText
                                                in charV >>= String.charAt 0 <#> G.fromChar <#> G.Single <#> G.Char
                                            _ -> Nothing
                                    OneOf _ 3 ioomatch -> -- any char
                                        case ioomatch of
                                            Rule _ "anyChar" _ ->
                                                Just $ G.Char G.Any
                                            _ -> Nothing
                                    _ -> Nothing
                            _ ->
                                Nothing
                    OneOf _ 3 oomatch -> -- text
                        case oomatch of
                            Rule _ "text" tMatch ->
                                case tMatch of
                                    Many _ sequence ->
                                        case Array.index sequence 1 of
                                            Just (Many _ strCharMatches) ->
                                                Just $ G.Text $ collectContent strCharMatches
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                    OneOf _ 4 oomatch -> -- repSep
                        case oomatch of
                            Rule _ "pepSep" repSepMatch ->
                                case repSepMatch of
                                    Many _ sequence ->
                                        let
                                            mbRep =
                                                Array.index sequence 2 >>= extractRule
                                            mbSep =
                                                Array.index sequence 4 >>= extractRule
                                        in
                                            G.RepSep <$> mbRep <*> mbSep
                                    _ -> Nothing
                            _ -> Nothing
                    OneOf _ 5 oomatch -> -- placeholder
                        case oomatch of
                            Rule _ "placeholder" _ ->
                                Just G.Placeholder
                            _ -> Nothing
                    OneOf _ 6 oomatch -> -- ref
                        case oomatch of
                            Rule _ "ref" refMatch ->
                                case refMatch of
                                    Many _ items ->
                                        let
                                            mbCaptureName =
                                                case Array.index items 1 of
                                                    Just (OneOf _ 0 cnScMatch) ->
                                                        case cnScMatch of
                                                            Many _ submatches ->
                                                                Array.index submatches 0 <#> collectText
                                                            _ -> Nothing

                                                    _ -> Nothing
                                            mbRuleName = Array.index items 1 <#> collectText
                                        in
                                            G.Ref mbCaptureName <$> mbRuleName
                                    _ -> Nothing
                            _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

        collectText :: Match String -> String
        collectText = case _ of
            Rule text _ _ -> text
            OneOf text _ _ -> text
            Many text _ -> text
            Value text -> text

        collectContent :: Array (Match String) -> String
        collectContent = fold <<< map collectText


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