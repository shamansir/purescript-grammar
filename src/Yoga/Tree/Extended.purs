module Yoga.Tree.Extended where

import Prelude (($), (<<<), map)

import Control.Comonad.Cofree (head, tail) as Y
import Data.Maybe (Maybe, fromMaybe)
import Data.Array (catMaybes) as Array
import Data.Traversable (sequence)

import Yoga.Tree (Tree, leaf, mkTree, setNodeValue) as Y


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


set :: forall n. n -> Tree n -> Tree n
set = Y.setNodeValue


catMaybes :: forall n. n -> Tree (Maybe n) -> Tree n
catMaybes rootDefault =
    break deleteMaybes
    where

        deleteMaybes :: Maybe n -> Array (Tree (Maybe n)) -> Tree n
        deleteMaybes mbVal =
            node (fromMaybe rootDefault mbVal) <<< Array.catMaybes <<< map sequence
