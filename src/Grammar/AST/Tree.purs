module Grammar.AST.Tree where

import Prelude

import Control.Comonad.Cofree (head, tail) as Y

import Yoga.Tree (Tree, leaf, mkTree) as Y


type Tree n = Y.Tree n


leaf :: forall n. n -> Tree n
leaf = Y.leaf


node :: forall n. n -> Array (Tree n) -> Tree n
node = Y.mkTree


value :: forall n. Tree n -> n
value = Y.head


children :: forall n. Tree n -> Array (Tree n)
children = Y.tail


break :: forall n a. (n -> Array (Tree n) -> a) -> Tree n -> a
break f t =
    f (Y.head t) $ Y.tail t


{- break :: forall n a. { leaf :: n -> a, node :: n -> Array (Tree n) -> a } -> Tree n -> a
break b = case _ of
    Leaf n -> b.leaf n
    Node n items -> b.node n items
-}


-- toYogaTree :: forall n. Tree n -> Y.Tree n
-- toYogaTree = case _ of
--     Leaf n -> Y.leaf n
--     Node n children -> Y.mkTree n $ toYogaTree <$> children
