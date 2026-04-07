module RegexToEnfa
open Io.ce_regex_to_enfa
open RegexAST
open FSharp.Text.Lexing

let analysis (input: Input) : Output =
        let lexbuf = LexBuffer<char>.FromString input.regex
        let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
        printfn "Parsed AST: %A" ast
        failwith "Module not yet implemented"
 
