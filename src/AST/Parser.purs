module AST.Parser where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map

import Grammar (Grammar, Rule(..), AST(..), Tree(..), Attempt(..), ASTNode, RuleSet, At(..))
import Grammar (set, main) as G


type State = { position :: Int, next :: String }


newtype Parser a = Parser (State -> ASTNode a)


type P a = Parser a


parse :: forall a. Grammar -> (Rule -> a) -> String -> AST a
parse grammar f = runParser $ parseRule (G.set grammar) f Main $ G.main grammar


runParser :: forall a. P a -> String -> AST a
runParser (Parser p) str =
    AST $ p { position : 0, next : str }


parseRule :: forall a. RuleSet -> (Rule -> a) -> At -> Rule -> P a
parseRule set f at rule =
    Parser $ \state -> Leaf { rule : Ref Nothing "main", result : Match { start : 0, end : 0 } $ f (Ref Nothing "main") }