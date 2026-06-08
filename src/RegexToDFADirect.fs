module RegexToDfaDirect
open Io.ce_regex_to_dfa_direct
open RegexAST
open RegexToEnfa
open FSharp.Text.Lexing

let analysis (input: Input) : Output =
    let lexbuf = LexBuffer<char>.FromString input.regex
    let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
    let enfa = buildENFA ast

    let mutable alphabet: Set<char> = Set.empty
    for (_, ch, _) in enfa.transitions do
        if ch <> eps then alphabet <- Set.add ch alphabet

    let epsClosure (start: string) =
        let mutable closure = Set.singleton start
        let mutable worklist = [start]
        while not worklist.IsEmpty do
            let s = worklist.Head
            worklist <- worklist.Tail
            for (from, ch, dst) in enfa.transitions do
                if from = s && ch = eps && not (Set.contains dst closure) then
                    closure <- Set.add dst closure
                    worklist <- dst :: worklist
        closure

    let epsClosureSet states =
        let mutable result = Set.empty
        for s in states do
            result <- Set.union result (epsClosure s)
        result

    // Subset construction
    let initialState = epsClosure enfa.startState
    let mutable visited: Set<Set<string>> = Set.empty
    let mutable dfaTrans: (Set<string> * char * Set<string>) list = []
    let mutable worklist = [initialState]

    while not worklist.IsEmpty do
        let current = worklist.Head
        worklist <- worklist.Tail

        if not (Set.contains current visited) then
            visited <- Set.add current visited

            for ch in alphabet do
                let mutable reachable = Set.empty
                for s in current do
                    for (from, c, dst) in enfa.transitions do
                        if from = s && c = ch then
                            reachable <- Set.add dst reachable

                let next = epsClosureSet reachable
                dfaTrans <- (current, ch, next) :: dfaTrans
                worklist <- next :: worklist


    let mutable acceptingStates: Set<Set<string>> = Set.empty
    for s in visited do
        if Set.contains enfa.acceptState s then
            acceptingStates <- Set.add s acceptingStates


    let mutable nameList: (Set<string> * string) list = []
    let mutable nameCounter = 0
    let getName subset =
        match List.tryFind (fun (s, _) -> s = subset) nameList with
        | Some (_, n) -> n
        | None ->
            let n = sprintf "d%d" nameCounter
            nameCounter <- nameCounter + 1
            nameList <- (subset, n) :: nameList
            n


    let sb = System.Text.StringBuilder()
    let app (s: string) = sb.Append(s) |> ignore
    app "digraph Automaton {\n  rankdir=LR;\n"
    for s in visited do
        let attrs = [
            if s = initialState               then yield "isInitial=true"
            if Set.contains s acceptingStates then yield "isAccepting=true"
        ]
        if attrs.IsEmpty then app (sprintf "  \"%s\";\n" (getName s))
        else app (sprintf "  \"%s\" [%s];\n" (getName s) (String.concat ", " attrs))
    app "\n"
    let mutable compacted: (Set<string> * Set<string> * Set<char>) list = []
    for (from, ch, dst) in dfaTrans do
        let mutable found = false
        let mutable newCompacted = []
        for (f, d, labels) in compacted do
            if f = from && d = dst then
                newCompacted <- (f, d, Set.add ch labels) :: newCompacted
                found <- true
            else
                newCompacted <- (f, d, labels) :: newCompacted
        if not found then
            newCompacted <- (from, dst, Set.singleton ch) :: newCompacted
        compacted <- List.rev newCompacted

    for (from, dst, labels) in compacted do
        let label = String.concat "," (List.map (fun c -> string c) (Set.toList labels))
        app (sprintf "  \"%s\" -> \"%s\" [label=\"%s\"];\n" (getName from) (getName dst) label)
    app "}"
    { dot = sb.ToString() }
