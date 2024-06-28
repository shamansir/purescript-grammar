module Grammar.Self.Extract where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (index, catMaybes, find) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.FunctorWithIndex (mapWithIndex)

import Grammar (Grammar)
import Grammar (Rule) as Grammar
import Grammar (Rule(..), RuleName) as G
import Grammar.AST (AST(..), At, ASTNode)
import Grammar.AST (root, Cell, Attempt(..), Range) as AST

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (children, value) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (traverse) as Tree


{-data Match a
    = RuleDef String At Grammar.Rule a
    | Rule At Grammar.Rule a
    | Text String a -}


data Match a
    = Rule a G.RuleName (Match a)
    | Choice a Int (Match a)
    | Sequence a (Array (Match a))
    | Reps a (Array (Match a))
    | Text a


{- TODO FIXME
data Match a
    = Rule G.RuleName (Match a)
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
                G.Sequence _ -> Just $ Sequence a $ Array.catMaybes $ (convert <$> Tree.children node)
                G.Choice _ ->
                    Tree.children node
                        # mapWithIndex (/\)
                        # Array.find (Tuple.snd >>> _isMatch)
                        <#> map convert
                        >>= \(n /\ mbMatch) -> mbMatch <#> Choice a n
                G.RepSep _ _ -> Just $ Reps a $ Array.catMaybes $ (convert <$> Tree.children node)
                G.Ref mbCapture ruleName ->
                    Array.index (Tree.children node) 0
                        >>= convert
                        <#> Rule a (fromMaybe ruleName mbCapture)
                G.Text _ -> Just $ Text a
                G.Char _ -> Just $ Text a
                G.Placeholder -> Nothing
                G.None -> Nothing
        AST.Fail _ _ -> Nothing


-- extract :: forall a. AST a -> Grammar
-- extract from = ?wh


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