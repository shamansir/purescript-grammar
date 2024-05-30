module Grammar.Parser where

import Prelude


import Grammar (Grammar)
import Grammar (empty) as Grammar

import Parsing (Parser)


parser :: Parser String Grammar
parser = pure Grammar.empty