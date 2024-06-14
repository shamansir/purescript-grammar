module AST.Parser where

import Prelude

import Data.Maybe (Maybe(..))

import Grammar (Grammar, Rule(..), AST(..), Tree(..), Attempt(..))


parse :: forall a. Grammar -> (Rule -> a) -> String -> AST a
parse _ f _ = AST $ Leaf { rule : Ref Nothing "main", result : Match { start : 0, end : 0 } $ f (Ref Nothing "main") }
-- TODO