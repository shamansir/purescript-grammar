module Grammar.Self.Extract where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (catMaybes) as Array
import Data.Tuple.Nested ((/\))
import Data.Map (fromFoldable, lookup, delete) as Map
import Data.String.CodeUnits (charAt) as String

import Grammar (Grammar(..))
import Grammar (Rule, empty) as Grammar
import Grammar (Rule(..), RuleName, WhichChar(..), fromChar, fromString) as G
import Grammar.AST (AST)
import Grammar.AST (root, toPoint) as AST

-- import Debug as Debug

import Grammar.AST.Point (Point)
import Grammar.AST.Point as P


extract :: AST String -> Grammar
extract =
    -- let
    --     _ = Debug.spy "ast" $ show ast
    --     _ = Debug.spy "convert" $ show $ convert $ AST.root ast
    -- in ast # AST.root # convert # flip bind load # fromMaybe Grammar.empty
    AST.root >>> AST.toPoint >>> flip bind P.points >>> flip bind load >>> fromMaybe Grammar.empty


load :: Array (Point String) -> Maybe Grammar
load ruleDefnMatches =
    loadGrammar $ Array.catMaybes $ flip bind extractRuleDef <$> P.ifChosen 0 <$> ruleDefnMatches

    where

        loadGrammar :: Array { ruleName :: G.RuleName, rule :: Grammar.Rule } -> Maybe Grammar
        loadGrammar ruleDefs =
            let allRules = Map.fromFoldable $ (\{ ruleName, rule } -> ruleName /\ rule ) <$> ruleDefs
            in
                Map.lookup "main" allRules <#> \mainRule -> Grammar mainRule $ Map.delete "main" allRules

        extractRuleDef :: Point String -> Maybe { ruleName :: G.RuleName, rule :: Grammar.Rule }
        extractRuleDef match = do
            ruleDefn <- P.ifRule "ruleDefn" match
            ruleName <- P.take 0 ruleDefn >>= P.valueIfRule "ident"
            rule     <- P.take 4 ruleDefn >>= extractRule "rule"
            pure { ruleName, rule }


        extractRule :: String -> Point String -> Maybe Grammar.Rule
        extractRule expectedName match =
            P.ifRule expectedName match
                >>= P.pointChosen \n oomatch ->
                case n of
                    0 -> seq oomatch
                    1 -> choice oomatch
                    2 -> charRule' oomatch
                    3 -> text oomatch
                    4 -> repSep oomatch
                    5 -> oomatch # P.ifRule "placeholder" $> pure G.Placeholder
                    6 -> ref oomatch
                    _ -> Nothing


        ref :: Point String -> Maybe G.Rule
        ref oomatch = do
            refMatch <- P.ifRule "ref" oomatch
            let mbCaptureName = P.take 0 refMatch >>= P.ifChosen 0 >>= P.take 0 <#> P.collectText
            ruleName <- P.take 1 refMatch <#> P.collectText
            pure $ G.Ref mbCaptureName ruleName


        repSep :: Point String -> Maybe G.Rule
        repSep oomatch = do
            repSepMatch <- P.ifRule "repSep" oomatch
            rep <- P.take 2 repSepMatch >>= extractRule "rep"
            sep <- P.take 4 repSepMatch >>= extractRule "sep"
            pure $ G.RepSep rep sep


        seq :: Point String -> Maybe G.Rule
        seq oomatch = do
            seqMatch <- P.ifRule "seq" oomatch
            ruleMatches <- P.take 2 seqMatch >>= P.points
            pure $ G.Sequence $ Array.catMaybes $ extractRule "rule" <$> ruleMatches


        choice :: Point String -> Maybe G.Rule
        choice oomatch = do
            chMatch <- P.ifRule "choice" oomatch
            ruleMatches <- P.take 2 chMatch >>= P.points
            pure $ G.Choice $ Array.catMaybes $ extractRule "rule" <$> ruleMatches


        text :: Point String -> Maybe G.Rule
        text oomatch = do
            tMatch <- P.ifRule "text" oomatch
            strCharMatches <- P.take 1 tMatch >>= P.points
            pure $ G.Text $ P.collectContent strCharMatches


        charRule' :: Point String -> Maybe G.Rule
        charRule' oomatch =
            P.ifRule "charRule" oomatch
                >>= P.pointChosen \n ioomatch ->
                case n of
                    0 -> do -- char range
                        crMatch  <- P.ifRule "charRange" ioomatch
                        fromChar <- P.take 1 crMatch <#> P.collectText >>= String.charAt 0
                        toChar   <- P.take 3 crMatch <#> P.collectText >>= String.charAt 0
                        pure $ G.Char $ G.Range fromChar toChar
                    1 -> do -- not char
                        ncMatch  <- P.ifRule "notChar" ioomatch
                        charStr <- P.take 1 ncMatch <#> P.collectText <#> P._unescape
                        charx <- charStr # String.charAt 1 <#> G.fromChar
                        pure $ G.Char $ G.Not charx
                    2 -> do -- single char
                        scMatch  <- P.ifRule "singleChar" ioomatch
                        charStr <- P.take 1 scMatch <#> P.collectText
                        charx <- charStr # G.fromString
                        pure $ G.Char $ G.Single charx
                    3 -> do
                        _ <- P.ifRule "anyChar" ioomatch
                        pure $ G.Char G.Any
                    _ -> Nothing