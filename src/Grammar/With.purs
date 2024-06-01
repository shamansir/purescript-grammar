module Grammar.With where

import Prelude

import Grammar (Grammar, AST(..), Rule(..), CharRule(..), RuleName, Range)
import Grammar (main, find) as G

import Data.Either (Either(..))
import Data.Traversable (for)

import Parsing (Parser, ParseError, runParser)
import Parsing (position) as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as P
import Parsing.Combinators.Array as PA
import Parsing.String as P
import Parsing.String.Basic as P


type P a = Parser String a


parse :: forall a. Grammar -> (Rule -> a) -> String -> Either ParseError (AST a)
parse grammar f str =
    runParser str $ parseRule f "main" $ G.main grammar


parseRule :: forall a. (Rule -> a) -> RuleName -> Rule -> P (AST a)
parseRule f rname rule =
    case rule of
        Text text -> ql $ P.string text
        CharRule (Single ch) -> ql $ P.char ch
        Sequence rules ->
            qn $ for rules $ parseRule f "ch"
        _ -> pure Nil
    where
        ql :: forall z. P z -> P (AST a)
        ql = withRange leaf
        qn :: P (Array (AST a)) -> P (AST a)
        qn = withRange node
        leaf :: forall x. x -> Range -> AST a
        leaf _ rng = Leaf rname rule rng $ f rule
        node :: Array (AST a) -> Range -> AST a
        node rules rng = Node rname rule rng (f rule) rules
        withRange :: forall c z. (c -> Range -> z) -> P c -> P z
        withRange frng p = do
            posBefore <- P.position
            res <- p
            posAfter <- P.position
            pure $ frng res { start : 0, end : 0 }
