module Yoga.Tree.Path where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (index, uncons, mapWithIndex, snoc, length, reverse) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String


-- TODO: duplicates `Zipper.Loc` / depth first ?
-- TODO: if not, use `Zipper.Loc` with `Path`


import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, children, node, value) as YX


newtype Path = Path (Array Int)


toArray :: Path -> Array Int
toArray (Path array) = array


fill :: forall a. Tree a -> Tree (Path /\ a)
fill = YX.break \a -> YX.node (Path [] /\ a) <<< Array.mapWithIndex (fill' [])
    where
        fill' path idx =
            let curPath = Array.snoc path idx
            in YX.break \a -> YX.node (Path curPath /\ a) <<< Array.mapWithIndex (fill' curPath)


find :: forall a. Path -> Tree a -> Maybe (Tree a)
find (Path path) =
    if (Array.length path > 0)
    then flip find' $ Array.reverse path
    else Just
    where
        find' parent rempath =
            Array.uncons rempath >>=
                \{ head, tail } ->
                    Array.index (YX.children parent) head
                        >>= flip find' tail


with :: forall a. Path -> (Tree a -> Tree a) -> Tree a -> Tree a
with (Path path) f =
    if (Array.length path > 0)
    then flip with' $ Array.reverse path
    else f
    where
        with' node rempath =
            case Array.uncons rempath of
                Just { head, tail } ->
                    YX.node (YX.value node)
                        $ Array.mapWithIndex (\idx child -> if idx == head then f child else with' child tail)
                        $ YX.children node
                Nothing -> node


traverse :: forall a b. (Path -> a -> Tree a -> b) -> Tree a -> Tree b
traverse f =
    traverse' []
    where
        traverse' path node =
            YX.break
                (\n ->
                    YX.node (f (Path path) n node)
                        <<< Array.mapWithIndex
                            (traverse' <<< Array.snoc path)
                )
                node


instance Show Path where
    show (Path path) =
        case path of
            [] -> "<root>"
            indices -> String.joinWith ":" $ show <$> indices