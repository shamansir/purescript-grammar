module Grammar.Self.Parser where

import Prelude


import Grammar (Grammar, Rule)
import Grammar.Build ((:-))
import Grammar.Build as B


grammar :: Grammar
grammar =
    B.grammar
        main
        [ "comment" :- comment
        , "ruleDefn" :- ruleDefn

        , "rule" :- rule

        , "seq" :- seq
        , "choice" :- choice
        , "ref" :- ref
        , "captureName" :- captureName
        , "ruleName" :- ruleName
        , "text" :- text
        , "repSep" :- repSep
        , "repSepKW" :- repSepKW

        , "charRule" :- charRule
        , "charRange" :- charRange
        , "notChar" :- notChar
        , "singleChar" :- singleChar
        , "anyChar" :- anyChar

        , "ident" :- ident
        , "ws" :- ws
        , "commaSpace" :- commaSpace
        , "placeholder" :- placeholder

        , "alphaNum" :- alphaNum
        , "alpha" :- alpha
        , "num" :- num
        , "stringChar" :- stringChar
        , "commentChar" :- commentChar
        ]


main :: Rule
main =
    B.repSep
        { rep :
            B.choice
                [ B.ref "ruleDefn"
                , B.ref "comment"
                ]
        , sep : B.ref "ws"
        }


comment :: Rule
comment =
    B.sequence
        [ B.text "#"
        , B.repSep
            { rep : B.ref "commentChar"
            , sep : B.text ""
            }
        ]


ruleDefn :: Rule
ruleDefn =
    B.sequence
        [ B.ref "ident"
        , B.ref "ws"
        , B.text ":-"
        , B.ref "ws"
        , B.ref "rule"
        , B.text "."
        ]


rule :: Rule
rule =
    B.choice
        [ B.ref "seq"
        , B.ref "choice"
        , B.ref "charRule"
        , B.ref "text"
        , B.ref "repSep"
        , B.ref "placeholder"
        , B.ref "ref"
        ]


seq :: Rule
seq =
    B.sequence
        [ B.text "["
        , B.ref "ws"
        , B.repSep
            { rep : B.ref "rule"
            , sep : B.sequence
                [ B.ref "ws"
                , B.text ","
                , B.ref "ws"
                ]
            }
        , B.ref "ws"
        , B.text "]"
        ]


choice :: Rule
choice =
    B.sequence
        [ B.text "("
        , B.ref "ws"
        , B.repSep
            { rep : B.ref "rule"
            , sep : B.sequence
                [ B.ref "ws"
                , B.text "|"
                , B.ref "ws"
                ]
            }
        , B.ref "ws"
        , B.text ")"
        ]


-- _ref = B.ref "ref"
ref :: Rule
ref =
    B.sequence
        [ B.choice
            [ B.sequence
                [ B.ref "captureName"
                , B.text ":"
                ]
            , B.text ""
            ]
        , B.ref "ruleName"
        ]


captureName :: Rule
captureName = B.ref "ident"


ruleName :: Rule
ruleName = B.ref "ident"


text :: Rule
text =
    B.sequence
        [ B.text "\""
        , B.repSep
            { rep : B.ref "stringChar"
            , sep : B.text ""
            }
        , B.text "\""
        ]


repSep :: Rule
repSep =
    B.sequence
        [ B.ref "repSepKW"
        , B.text "("
        , B.refAs "rep" "rule"
        , B.ref "commaSpace"
        , B.refAs "sep" "rule"
        , B.text ")"
        ]


repSepKW :: Rule
repSepKW =
    B.text "repSep"


charRule :: Rule
charRule =
    B.choice
        [ B.ref "charRange"
        , B.ref "notChar"
        , B.ref "singleChar"
        , B.ref "anyChar"
        ]


charRange :: Rule
charRange =
    B.sequence
        [ B.text "["
        , B.refAs "from" "alphaNum"
        , B.text "-"
        , B.refAs "to" "alphaNum"
        , B.text "]"
        ]


notChar :: Rule
notChar =
    B.sequence
        [ B.char '^'
        , B.ref "charRule"
        ]


singleChar :: Rule
singleChar =
    B.sequence
        [ B.text "'"
        , B.choice
            [ B.sequence
                [ B.char' $ B.escape '\\'
                , B.char 'n'
                ]
            , B.sequence
                [ B.char' $ B.escape '\\'
                , B.char' $ B.escape '\\'
                ]
            , B.anyChar
            ]
        , B.text "'"
        ]


anyChar :: Rule
anyChar =
    B.text "."


ident :: Rule
ident =
    B.repSep
        { rep : B.ref "alpha"
        , sep : B.text ""
        }


ws :: Rule
ws =
    B.repSep
        { rep :
            B.choice
                [ B.text " "
                , B.text "\n"
                ]
        , sep : B.text ""
        }


commaSpace :: Rule
commaSpace =
    B.sequence
        [ B.text ","
        , B.ref "ws"
        ]


placeholder :: Rule
placeholder =
    B.text "???"


alphaNum :: Rule
alphaNum =
    B.choice
        [ B.ref "alpha"
        , B.ref "num"
        ]


alpha :: Rule
alpha =
    B.choice
        [ B.range { from : 'a', to : 'z' }
        , B.range { from : 'A', to : 'Z' }
        ]


num :: Rule
num =
    B.range { from : '0', to : '9' }


stringChar :: Rule
stringChar =
    B.choice
        [ B.sequence
            [ B.char' $ B.escape '\\'
            , B.char '"'
            ]
        , B.notChar '"'
        ]


commentChar :: Rule
commentChar =
    B.notChar' $ B.escape 'n'
