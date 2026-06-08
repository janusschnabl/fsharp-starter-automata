module RegexToEnfa
open Io.ce_regex_to_enfa
open RegexAST
open FSharp.Text.Lexing

type ENFA =
    {
        states: Set<string>
        alphabet: Set<char>
        transitions: Map<string, Map<char, Set<string>>>
        startState: Set<string>
        acceptStates: Set<string>
    }

let eps = 'ε'

let fresh (c: int ref) =
    let n = !c
    c := n + 1
    sprintf "q%d" n

// t is last so |> can chain multiple addTrans calls
let addTrans from ch ``to`` t =
    let inner = t |> Map.tryFind from |> Option.defaultValue Map.empty
    let dests = inner |> Map.tryFind ch |> Option.defaultValue Set.empty |> Set.add ``to``
    t |> Map.add from (inner |> Map.add ch dests)

// Each function returns (start, accept, states, transitions)

let thompsonEmpty c t =
    let s, a = fresh c, fresh c
    s, a, Set.ofList [s; a], t |> addTrans s eps a

let thompsonChar c t ch =
    let s, a = fresh c, fresh c
    s, a, Set.ofList [s; a], t |> addTrans s ch a

let thompsonConcat (s1, a1, st1, _) (s2, a2, st2, t2) =
    s1, a2, Set.union st1 st2, t2 |> addTrans a1 eps s2

let thompsonAlternation c (s1, a1, st1, _) (s2, a2, st2, t2) =
    let s, a = fresh c, fresh c
    let t3 = t2 |> addTrans s eps s1 |> addTrans s eps s2
                |> addTrans a1 eps a |> addTrans a2 eps a
    s, a, Set.union (Set.union st1 st2) (Set.ofList [s; a]), t3

let thompsonStar c (rs, ra, st, t1) =
    let s, a = fresh c, fresh c
    let t2 = t1 |> addTrans s eps rs |> addTrans s eps a
                |> addTrans ra eps rs |> addTrans ra eps a
    s, a, Set.union st (Set.ofList [s; a]), t2

let thompsonPlus c (rs, ra, st, t1) =
    let s, a = fresh c, fresh c
    let t2 = t1 |> addTrans s eps rs
                |> addTrans ra eps rs
                |> addTrans ra eps a
    s, a, Set.union st (Set.ofList [s; a]), t2

let rec thompson c t ast =
    match ast with
    | Empty             -> thompsonEmpty c t
    | Char ch           -> thompsonChar c t ch
    | Concat(r1, r2)    ->
        let f1 = thompson c t r1
        let _, _, _, t1 = f1
        thompsonConcat f1 (thompson c t1 r2)
    | Alternation(r1,r2)->
        let f1 = thompson c t r1
        let _, _, _, t1 = f1
        thompsonAlternation c f1 (thompson c t1 r2)
    | Star r            -> thompsonStar c (thompson c t r)
    | Plus r            -> thompsonPlus c (thompson c t r)

let toDot start accept states trans =
    let sb = System.Text.StringBuilder()
    let app (s: string) = sb.Append(s) |> ignore
    app "digraph {\n    rankdir=LR;\n    \"\" [shape=none];\n"
    for s in states do
        if s = accept then app (sprintf "    %s [shape=doublecircle];\n" s)
        else app (sprintf "    %s [shape=circle];\n" s)
    app (sprintf "    \"\" -> %s;\n" start)
    for KeyValue(from, inner) in trans do
        for KeyValue(ch, dests) in inner do
            for dest in dests do
                app (sprintf "    %s -> %s [label=\"%s\"];\n" from dest (string ch))
    app "}"
    sb.ToString()

let buildENFA (ast: regex) : ENFA =
    let c = ref 0
    let start, accept, states, trans = thompson c Map.empty ast
    {
        states = states
        alphabet = Set.empty
        transitions = trans
        startState = Set.singleton start
        acceptStates = Set.singleton accept
    }

let analysis (input: Input) : Output =
    let lexbuf = LexBuffer<char>.FromString input.regex
    let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
    let c = ref 0
    let start, accept, states, trans = thompson c Map.empty ast
    { dot = toDot start accept states trans }
