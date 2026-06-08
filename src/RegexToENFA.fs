module RegexToEnfa
open Io.ce_regex_to_enfa
open RegexAST
open FSharp.Text.Lexing

type ENFA =
    {
        states: Set<string>
        transitions: (string * char * string) list
        startState: string
        acceptState: string
    }

let eps = 'ε'

let build ast =
    let mutable counter = 0
    let mutable states: Set<string> = Set.empty
    let mutable transition: (string * char * string) list = []
    let mutable initialStates: Set<string> = Set.empty
    let mutable acceptingStates: Set<string> = Set.empty

    let newState () =
        let s = sprintf "k%d" counter
        counter <- counter + 1
        states <- Set.add s states
        s

    let addTransition from label destination =
        transition <- (from, label, destination) :: transition

    let rec thompsons ast =
        match ast with
        | Empty ->
            let s, a = newState(), newState()
            addTransition s eps a
            initialStates <- Set.ofList [s; a]
            acceptingStates <- Set.ofList [s; a]
            s, a
        | Char ch ->
            let s, a = newState(), newState()
            addTransition s ch a
            s, a
        | Concat(r1, r2) ->
            let (s1, a1) = thompsons r1
            let (s2, a2) = thompsons r2
            addTransition a1 eps s2
            s1, a2
        | Alternation(r1, r2) ->
            let (s1, a1) = thompsons r1
            let (s2, a2) = thompsons r2
            let s, a = newState(), newState()
            addTransition s eps s1; addTransition s eps s2
            addTransition a1 eps a; addTransition a2 eps a
            s, a
        | Star r ->
            let (rs, ra) = thompsons r
            let s, a = newState(), newState()
            addTransition s eps rs
            addTransition ra eps rs; addTransition ra eps a
            s, a
        | Plus r ->
            let (rs, ra) = thompsons r
            let s, a = newState(), newState()
            addTransition s eps rs
            addTransition ra eps rs; addTransition ra eps a
            s, a

    let start, accept = thompsons ast
    if initialStates.IsEmpty  then initialStates  <- Set.singleton start
    if acceptingStates.IsEmpty then acceptingStates <- Set.singleton accept
    start, accept, initialStates, acceptingStates, states, transition

let toDot start accept (initialStates: Set<string>) (acceptingStates: Set<string>) states transition =
    let sb = System.Text.StringBuilder()
    let app (s: string) = sb.Append(s) |> ignore
    app "digraph Automaton {\n  rankdir=LR;\n"
    for s in states do
        let attrs = [
            if initialStates.Contains s  then yield "isInitial=true"
            if acceptingStates.Contains s then yield "isAccepting=true"
        ]
        if attrs.IsEmpty then app (sprintf "  \"%s\";\n" s)
        else app (sprintf "  \"%s\" [%s];\n" s (String.concat ", " attrs))
    app "\n"
    for (from, ch, dst) in transition do
        app (sprintf "  \"%s\" -> \"%s\" [label=\"%s\"];\n" from dst (string ch))
    app "}"
    sb.ToString()

let buildENFA (ast: regex) : ENFA =
    let start, accept, _, _, states, transition = build ast
    { states=states; transitions=transition; startState=start; acceptState=accept }

let analysis (input: Input) : Output =
    let lexbuf = LexBuffer<char>.FromString input.regex
    let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
    let start, accept, initialStates, acceptingStates, states, transition = build ast
    { dot = toDot start accept initialStates acceptingStates states transition }
