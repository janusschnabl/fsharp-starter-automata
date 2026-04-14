module RegexToEnfa
open Io.ce_regex_to_enfa
open RegexAST
open FSharp.Text.Lexing
open System.Collections.Generic

/// Represents an NFA with epsilon transitions
type EpsilonNFA = {
    states: Set<int>
    startState: int
    acceptStates: Set<int>
    transitions: Map<(int * char option), int list>
}

/// Thompson construction result - returns (nfa, start state, accept state)
type ThompsonResult = EpsilonNFA * int * int

/// Counter for generating unique state IDs
let mutable stateCounter = 0

let freshState () =
    let id = stateCounter
    stateCounter <- stateCounter + 1
    id

let resetStateCounter () =
    stateCounter <- 0

/// Create an NFA for a single character
let charNFA (c: char) : ThompsonResult =
    let start = freshState ()
    let accept = freshState ()
    let transition = Map.empty.Add((start, Some c), [accept])
    let nfa = { states = set [start; accept]; startState = start; acceptStates = set [accept]; transitions = transition }
    (nfa, start, accept)

/// Create an NFA for empty string (epsilon)
let emptyNFA () : ThompsonResult =
    let start = freshState ()
    let accept = freshState ()
    let transition = Map.empty.Add((start, None), [accept])
    let nfa = { states = set [start; accept]; startState = start; acceptStates = set [accept]; transitions = transition }
    (nfa, start, accept)

/// Merge two NFAs in sequence (concatenation)
let concatNFA (nfa1: ThompsonResult) (nfa2: ThompsonResult) : ThompsonResult =
    let (n1, s1, a1) = nfa1
    let (n2, s2, a2) = nfa2
    
    let mergedStates = Set.union n1.states n2.states
    
    let newTransition = 
        Map.fold (fun acc key value -> Map.add key value acc) n1.transitions n2.transitions
        |> fun m -> 
            match Map.tryFind (a1, None) m with
            | Some existing -> Map.add (a1, None) (existing @ [s2]) m
            | None -> Map.add (a1, None) [s2] m
    
    let nfa = { states = mergedStates; startState = s1; acceptStates = n2.acceptStates; transitions = newTransition }
    (nfa, s1, a2)

/// Merge two NFAs in parallel (alternation)
let altNFA (nfa1: ThompsonResult) (nfa2: ThompsonResult) : ThompsonResult =
    let (n1, s1, a1) = nfa1
    let (n2, s2, a2) = nfa2
    
    let newStart = freshState ()
    let newAccept = freshState ()
    
    let mergedStates = Set.union n1.states n2.states |> Set.add newStart |> Set.add newAccept
    
    let newTransition = 
        Map.fold (fun acc key value -> Map.add key value acc) n1.transitions n2.transitions
        |> fun m ->
            let m' = 
                match Map.tryFind (newStart, None) m with
                | Some existing -> Map.add (newStart, None) (existing @ [s1; s2]) m
                | None -> Map.add (newStart, None) [s1; s2] m
            let m'' = 
                match Map.tryFind (a1, None) m' with
                | Some existing -> Map.add (a1, None) (existing @ [newAccept]) m'
                | None -> Map.add (a1, None) [newAccept] m'
            match Map.tryFind (a2, None) m'' with
            | Some existing -> Map.add (a2, None) (existing @ [newAccept]) m''
            | None -> Map.add (a2, None) [newAccept] m''
    
    let nfa = { states = mergedStates; startState = newStart; acceptStates = set [newAccept]; transitions = newTransition }
    (nfa, newStart, newAccept)

/// Apply Kleene star (zero or more)
let starNFA (nfa1: ThompsonResult) : ThompsonResult =
    let (n1, s1, a1) = nfa1
    
    let newStart = freshState ()
    let newAccept = freshState ()
    
    let mergedStates = Set.add newStart (Set.add newAccept n1.states)
    
    // Add epsilon from newStart to s1 and to newAccept (bypass)
    let t1 = n1.transitions |> Map.add (newStart, None) [s1; newAccept]
    
    // Add epsilon from a1 to newAccept and back to s1 (loop)
    let newTransition = 
        match Map.tryFind (a1, None) t1 with
        | Some existing -> Map.add (a1, None) (existing @ [newAccept; s1]) t1
        | None -> Map.add (a1, None) [newAccept; s1] t1
    
    let nfa = { states = mergedStates; startState = newStart; acceptStates = set [newAccept]; transitions = newTransition }
    (nfa, newStart, newAccept)

/// Apply Plus (one or more) - r+ is just r.r*
let plusNFA (nfaBase: ThompsonResult) : ThompsonResult =
    let (n1, s1, a1) = nfaBase
    
    let newStart = freshState ()
    let newAccept = freshState ()
    
    let mergedStates = Set.add newStart (Set.add newAccept n1.states)
    
    // Add epsilon from newStart to s1 (enter first occurrence)
    let t1 = n1.transitions |> Map.add (newStart, None) [s1]
    
    // Add epsilon from a1 back to s1 (loop for zero or more additional) and to newAccept
    let newTransition = 
        match Map.tryFind (a1, None) t1 with
        | Some existing -> Map.add (a1, None) (existing @ [newAccept; s1]) t1
        | None -> Map.add (a1, None) [newAccept; s1] t1
    
    let nfa = { states = mergedStates; startState = newStart; acceptStates = set [newAccept]; transitions = newTransition }
    (nfa, newStart, newAccept)

/// Convert regex AST to Thompson NFA
let rec regexToNFA (regex: regex) : ThompsonResult =
    match regex with
    | Empty -> emptyNFA ()
    | Char c -> charNFA c
    | Concat (r1, r2) -> concatNFA (regexToNFA r1) (regexToNFA r2)
    | Alt (r1, r2) -> altNFA (regexToNFA r1) (regexToNFA r2)
    | Star r -> starNFA (regexToNFA r)
    | Plus r -> plusNFA (regexToNFA r)

/// Convert NFA to DOT format
let nfaToDot (nfa: EpsilonNFA) : string =
    let sb = System.Text.StringBuilder()
    sb.AppendLine("digraph NFA {") |> ignore
    sb.AppendLine("  rankdir=LR;") |> ignore
    
    for state in nfa.states do
        let isInitial = state = nfa.startState
        let isAccepting = Set.contains state nfa.acceptStates
        if isInitial && isAccepting then
            sb.AppendLine($"  {state} [isInitial=true, isAccepting=true];") |> ignore
        elif isInitial then
            sb.AppendLine($"  {state} [isInitial=true];") |> ignore
        elif isAccepting then
            sb.AppendLine($"  {state} [isAccepting=true];") |> ignore
        else
            sb.AppendLine($"  {state};") |> ignore
    
    for KeyValue((state, label), nextStates) in nfa.transitions do
        let labelStr = match label with
                       | Some c -> c.ToString()
                       | None -> "ε"
        for nextState in nextStates do
            sb.AppendLine($"  {state} -> {nextState} [label=\"{labelStr}\"];") |> ignore
    
    sb.AppendLine("}") |> ignore
    sb.ToString()

let analysis (input: Input) : Output =
    resetStateCounter ()
    let lexbuf = LexBuffer<char>.FromString input.regex
    try
        let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
        let (nfa, _, _) = regexToNFA ast
        let dot = nfaToDot nfa
        { dot = dot }
    with
    | e -> 
        let errorDot = sprintf "digraph ERROR { label=\"Parse error: %s\"; }" e.Message
        { dot = errorDot }
 
