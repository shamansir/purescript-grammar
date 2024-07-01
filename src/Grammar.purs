module Grammar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, lookup, toUnfoldable) as Map
import Data.String (joinWith, length) as String
import Data.String.CodeUnits (singleton, charAt) as String
import Data.Tuple (uncurry) as Tuple
import Data.Array ((:))
import Data.Array (singleton) as Array

import Yoga.Tree.Extended (Tree)
import Yoga.Tree.Extended (node, leaf) as Tree


type RuleSet = Map RuleName Rule


data Grammar = Grammar Rule RuleSet


data Rule
    = Sequence (Array Rule)
    | Choice (Array Rule)
    | Ref (Maybe CaptureName) RuleName
    | Text String
    | Char WhichChar
    | RepSep Rule Rule
    | Placeholder
    | None


data WhichChar
    = Range Char Char
    | Not Char
    | Single Char
    | Any


type RuleName = String


type CaptureName = String


data RuleKnot
    = KRoot
    | KMain
    | KRuleDef RuleName
    | KSequence
    | KChoice
    | KRepSep
    | KRef (Maybe CaptureName) RuleName
    | KText String
    | KChar WhichChar
    | KPlaceholder
    | KNone


empty :: Grammar
empty = Grammar Placeholder Map.empty


from :: Rule -> Map RuleName Rule -> Grammar
from = Grammar


main :: Grammar -> Rule
main (Grammar m _) = m


find :: Grammar -> RuleName -> Maybe Rule
find = set >>> findIn


set :: Grammar -> RuleSet
set (Grammar _ s) = s


findIn :: RuleSet -> RuleName -> Maybe Rule
findIn = flip Map.lookup


expands :: Rule -> Boolean
expands = case _ of

    Sequence _ -> true
    Choice _ -> true
    Ref _ _ -> true -- FIXME: does `Ref` really expands? In the resulting AST it probably does
    RepSep _ _ -> true

    Text _ -> false
    Char _ -> false
    Placeholder -> false
    None -> false


expandsK :: RuleKnot -> Boolean
expandsK = case _ of

    KRoot -> true
    KMain -> true
    KRuleDef _ -> true

    KSequence -> true
    KChoice -> true
    KRepSep -> true

    KRef _ _ -> false
    KText _ -> false
    KChar _ -> false
    KPlaceholder -> false
    KNone -> false


toTree :: Grammar -> Tree RuleKnot
toTree (Grammar mainRule ruleSet) =
    Tree.node KRoot $ convertMainKnot mainRule : (Tuple.uncurry convertRuleDef <$> Map.toUnfoldable ruleSet)
    where
        convertMainKnot :: Rule -> Tree RuleKnot
        convertMainKnot = Tree.node KMain <<< Array.singleton <<< convertRule
        convertRuleDef :: RuleName -> Rule -> Tree RuleKnot
        convertRuleDef ruleName = Tree.node (KRuleDef ruleName) <<< Array.singleton <<< convertRule
        convertRule :: Rule -> Tree RuleKnot
        convertRule = case _ of
            Sequence children -> Tree.node KSequence $ convertRule <$> children
            Choice children -> Tree.node KChoice $ convertRule <$> children
            Ref mbCapture ruleName -> Tree.leaf $ KRef mbCapture ruleName
            RepSep rep sep -> Tree.node KRepSep [ convertRule rep, convertRule sep ]

            Text text -> Tree.leaf $ KText text
            Char which -> Tree.leaf $ KChar which
            Placeholder -> Tree.leaf KPlaceholder
            None -> Tree.leaf KNone


instance Show WhichChar where
    show = case _ of
        Range chA chB -> "[" <> String.singleton chA <> "-" <> String.singleton chB <> "]"
        Not char -> "^" <> show (Single char)
        Single char ->
            "'" <> String.singleton char <> "'"
        Any -> "."


instance Show Rule where
    show = case _ of
        Sequence rules -> "[" <> (String.joinWith "," $ show <$> rules) <> "]"
        Choice rules -> "(" <> (String.joinWith "|" $ show <$> rules) <> ")"
        Ref mbCapture ruleName ->
            case mbCapture of
                Just captureName -> captureName <> ":" <> ruleName
                Nothing -> ruleName
        Text text -> show text
        Char chrule -> show chrule
        RepSep repRule sepRule ->
            -- TODO: Some operator for RepSep
            "repSep(" <> show repRule <> "," <> show sepRule <> ")"
        Placeholder -> "???"
        None -> "x"


instance Show RuleKnot where
    show = case _ of
        KRoot -> "<root>"
        KMain -> "<main>"
        KRuleDef ruleName -> "<def:" <> ruleName <> ">"
        KSequence -> "<sequence>"
        KChoice -> "<choice>"
        KRepSep -> "<rep-sep>"
        KRef mbCapture ruleName ->
            "<ref:" <> ruleName
                    <> case mbCapture of
                            Just capture -> "@" <> capture
                            Nothing -> ""
                    <> ">"
        KText text -> "<text:(" <> text <> ")>"
        KChar which ->
            "<ch:" <>
            case which of
                Any -> "any"
                Range from to -> "[" <> String.singleton from <> "-" <> String.singleton to <> "]"
                Not chx -> "^" <> String.singleton chx
                Single chx -> String.singleton chx
            <> ">"
        KPlaceholder -> "<placeholder>"
        KNone -> "<?>"


instance Show Grammar where
    show (Grammar main others) =
        ruleLine "main" main <> "\n" <> String.joinWith "\n" (Map.toUnfoldable others <#> Tuple.uncurry ruleLine)
        where
            ruleLine name rule =
                name <> " :- " <> show rule <> "."
