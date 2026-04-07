// This file defines the abstract syntax tree (AST) for regular expressions.
module RegexAST

type regex =
    | Empty
    | Char of (char)
    | Concat of (regex * regex)
    | Alt of (regex * regex)
    | Star of (regex)
    | Plus of (regex)   


