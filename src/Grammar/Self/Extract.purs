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
    AST.root >>> AST.toPoint >>> (=<<) P.points >>> (=<<) load >>> fromMaybe Grammar.empty


load :: Array (Point String) -> Maybe Grammar
load ruleDefnPoints =
    loadGrammar $ Array.catMaybes $ (=<<) extractRuleDef <$> P.ifChosen 0 <$> ruleDefnPoints

    where

        loadGrammar :: Array { ruleName :: G.RuleName, rule :: Grammar.Rule } -> Maybe Grammar
        loadGrammar ruleDefs =
            let allRules = Map.fromFoldable $ (\{ ruleName, rule } -> ruleName /\ rule ) <$> ruleDefs
            in
                Map.lookup "main" allRules <#> \mainRule -> Grammar mainRule $ Map.delete "main" allRules

        extractRuleDef :: Point String -> Maybe { ruleName :: G.RuleName, rule :: Grammar.Rule }
        extractRuleDef point = do
            ruleDefn <- P.ifRule "ruleDefn" point
            ruleName <- P.take 0 ruleDefn >>= P.valueIfRule "ident"
            rule     <- P.take 4 ruleDefn >>= extractRule "rule"
            pure { ruleName, rule }


        extractRule :: String -> Point String -> Maybe Grammar.Rule
        extractRule expectedName point =
            P.ifRule expectedName point
                >>= P.pointChosen \n oopoint ->
                case n of
                    0 -> seq oopoint
                    1 -> choice oopoint
                    2 -> charRule' oopoint
                    3 -> text oopoint
                    4 -> repSep oopoint
                    5 -> oopoint # P.ifRule "placeholder" $> pure G.Placeholder
                    6 -> ref oopoint
                    _ -> Nothing


        ref :: Point String -> Maybe G.Rule
        ref oopoint = do
            refPoint <- P.ifRule "ref" oopoint
            let
                mbCaptureName =
                    P.take 0 refPoint
                        >>= P.ifChosen 0
                        >>= P.take 0
                        <#> P.collectText
            ruleName <- P.take 1 refPoint <#> P.collectText
            pure $ G.Ref mbCaptureName ruleName


        repSep :: Point String -> Maybe G.Rule
        repSep oopoint = do
            repSepPoint <- P.ifRule "repSep" oopoint
            rep <- P.take 2 repSepPoint >>= extractRule "rep"
            sep <- P.take 4 repSepPoint >>= extractRule "sep"
            pure $ G.RepSep rep sep


        seq :: Point String -> Maybe G.Rule
        seq oopoint = do
            seqPoint <- P.ifRule "seq" oopoint
            rulePoints <- P.take 2 seqPoint >>= P.points
            pure $ G.Sequence $ Array.catMaybes $ extractRule "rule" <$> rulePoints


        choice :: Point String -> Maybe G.Rule
        choice oopoint = do
            chPoint <- P.ifRule "choice" oopoint
            rulePoints <- P.take 2 chPoint >>= P.points
            pure $ G.Choice $ Array.catMaybes $ extractRule "rule" <$> rulePoints


        text :: Point String -> Maybe G.Rule
        text oopoint = do
            tPoint <- P.ifRule "text" oopoint
            strCharPoints <- P.take 1 tPoint >>= P.points
            pure $ G.Text $ P.collectContent strCharPoints


        charRule' :: Point String -> Maybe G.Rule
        charRule' oopoint =
            P.ifRule "charRule" oopoint
                >>= P.pointChosen \n ioopoint ->
                case n of
                    0 -> do -- char range
                        crPoint  <- P.ifRule "charRange" ioopoint
                        fromChar <- P.take 1 crPoint <#> P.collectText >>= String.charAt 0
                        toChar   <- P.take 3 crPoint <#> P.collectText >>= String.charAt 0
                        pure $ G.Char $ G.Range fromChar toChar
                    1 -> do -- not char
                        ncPoint  <- P.ifRule "notChar" ioopoint
                        charStr <- P.take 1 ncPoint <#> P.collectText <#> P._unescape
                        charx <- charStr # String.charAt 1 <#> G.fromChar
                        pure $ G.Char $ G.Not charx
                    2 -> do -- single char
                        scPoint  <- P.ifRule "singleChar" ioopoint
                        charStr <- P.take 1 scPoint <#> P.collectText
                        charx <- charStr # G.fromString
                        pure $ G.Char $ G.Single charx
                    3 -> do
                        _ <- P.ifRule "anyChar" ioopoint
                        pure $ G.Char G.Any
                    _ -> Nothing