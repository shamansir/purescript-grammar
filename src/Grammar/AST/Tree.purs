module Grammar.AST.Tree where

import Prelude


data Tree n -- TODO: use Yoga.Tree implementation
    = Leaf n
    | Node n (Array (Tree n))
