module RegexLexer

open FSharp.Text.Lexing
open System
open RegexGrammar/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token
