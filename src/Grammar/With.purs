module Grammar.With where

import Prelude

import Grammar (Grammar, AST(..), Rule(..))


parse :: forall a. Grammar -> (Rule -> a) -> String -> AST a
parse _ _ _ = Nil