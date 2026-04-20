module RegexToNfa
open Io.ce_regex_to_nfa
open RegexAST
open RegexToEnfa

/// Represents an NFA without epsilon transitions
type NFA = {
    states: Set<int>
    startState: int
    acceptStates: Set<int>
    transitions: Map<(int * char), int list>
}

/// Find all states that can reach accept states (backward reachability)
let backward_reachable (nfa: NFA) : Set<int> =
    let visited = ref (Set.ofSeq nfa.acceptStates)
    let stack = ref (Set.toList nfa.acceptStates)
    
    while !stack <> [] do
        let state = (!stack).Head
        stack := (!stack).Tail
        
        // Find all states that have transitions to current state
        Map.iter (fun (from_state: int, _symbol: char) targets ->
            if List.contains state targets && not (Set.contains from_state !visited) then
                visited := Set.add from_state !visited
                stack := from_state :: !stack
        ) nfa.transitions
    
    !visited

/// Find all reachable states from start state
let reachable_states (nfa: NFA) : Set<int> =
    let forward = ref Set.empty
    let stack = ref [nfa.startState]
    
    while !stack <> [] do
        let state = (!stack).Head
        stack := (!stack).Tail
        
        if not (Set.contains state !forward) then
            forward := Set.add state !forward
            
            // Find all transitions from current state
            Map.iter (fun (from_state: int, _symbol: char) targets ->
                if from_state = state then
                    for target_state in targets do
                        if not (Set.contains target_state !forward) then
                            stack := target_state :: !stack
            ) nfa.transitions
    
    let backward = backward_reachable nfa
    Set.intersect !forward backward

/// Prune NFA to only include reachable states
let prune_nfa (nfa: NFA) : NFA =
    let reachable = reachable_states nfa
    
    // Create mapping from old state indices to new ones
    let idx_mapping = ref Map.empty
    let mutable new_idx = 0
    
    for old_idx in Set.toList reachable do
        idx_mapping := Map.add old_idx new_idx !idx_mapping
        new_idx <- new_idx + 1
    
    // Remap transitions
    let new_transitions = ref Map.empty
    
    Map.iter (fun (from_state: int, symbol: char) (targets: int list) ->
        if Set.contains from_state reachable then
            match Map.tryFind from_state !idx_mapping with
            | Some new_from ->
                let new_targets = 
                    targets 
                    |> List.filter (fun target -> Set.contains target reachable)
                    |> List.choose (fun target -> Map.tryFind target !idx_mapping)
                if new_targets.Length > 0 then
                    new_transitions := Map.add (new_from, symbol) new_targets !new_transitions
            | None -> ()
    ) nfa.transitions
    
    // Remap accept states
    let new_accept_states = 
        Set.filter (fun state -> Set.contains state reachable) nfa.acceptStates
        |> Set.toList
        |> List.choose (fun state -> Map.tryFind state !idx_mapping)
        |> Set.ofList
    
    // Remap start state
    let new_start = 
        match Map.tryFind nfa.startState !idx_mapping with
        | Some s -> s
        | None -> nfa.startState
    
    {
        states = Set.ofList (List.init (Set.count reachable) id)
        startState = new_start
        acceptStates = new_accept_states
        transitions = !new_transitions
    }

/// Compute epsilon closure for a single state
let rec epsilon_closure (nfa: EpsilonNFA) (state: int) : Set<int> =
    let visited = ref (Set.empty)
    let worklist = ref [state]
    
    while !worklist <> [] do
        let current = (!worklist).Head
        worklist := (!worklist).Tail
        
        if not (Set.contains current !visited) then
            visited := Set.add current !visited
            
            // Find all epsilon transitions from current state
            match Map.tryFind (current, None) nfa.transitions with
            | Some targets ->
                for target in targets do
                    if not (Set.contains target !visited) then
                        worklist := target :: !worklist
            | None -> ()
    
    !visited

/// Compute epsilon closures for all states
let compute_epsilon_closures (nfa: EpsilonNFA) : Map<int, Set<int>> =
    Set.fold (fun acc state ->
        Map.add state (epsilon_closure nfa state) acc
    ) Map.empty nfa.states

/// Get outgoing symbols from a set of states
let outgoing_symbols (nfa: EpsilonNFA) (states: Set<int>) : Set<char> =
    Set.fold (fun acc state ->
        Map.fold (fun symbols (s: int, label: char option) _targets ->
            if s = state && label.IsSome then
                Set.add label.Value symbols
            else
                symbols
        ) acc nfa.transitions
    ) Set.empty states

/// Compute next states reachable via symbol from a state
let next_states (nfa: EpsilonNFA) (state: int) (symbol: char) : Set<int> =
    match Map.tryFind (state, Some symbol) nfa.transitions with
    | Some targets -> Set.ofList targets
    | None -> Set.empty

/// Convert epsilon NFA to NFA
let enfa_to_nfa (enfa: EpsilonNFA) : NFA =
    let epsilon_closures = compute_epsilon_closures enfa
    
    // Compute new transitions for each state
    let new_transitions = ref Map.empty
    
    for state in enfa.states do
        let closure = Map.find state epsilon_closures
        let symbols = outgoing_symbols enfa closure
        
        for symbol in symbols do
            let mutable targets = Set.empty
            
            for ec_state in closure do
                let reachable = next_states enfa ec_state symbol
                for reachable_state in reachable do
                    let target_closure = Map.find reachable_state epsilon_closures
                    targets <- Set.union targets target_closure
            
            let targetList = Set.toList targets
            if targetList.Length > 0 then
                new_transitions := Map.add (state, symbol) targetList !new_transitions
    
    // Determine accepting states (states whose epsilon closure intersects with accept states)
    let accepting_states = 
        Set.filter (fun state ->
            let closure = Map.find state epsilon_closures
            Set.intersect closure enfa.acceptStates |> Set.isEmpty |> not
        ) enfa.states
    
    {
        states = enfa.states
        startState = enfa.startState
        acceptStates = accepting_states
        transitions = !new_transitions
    }

/// Convert NFA to DOT format
let nfa_to_dot (nfa: NFA) : string =
    let sb = System.Text.StringBuilder()
    sb.AppendLine("digraph NFA {") |> ignore
    sb.AppendLine("  rankdir=LR;") |> ignore
    
    for state in nfa.states do
        let isInitial = state = nfa.startState
        let isAccepting = Set.contains state nfa.acceptStates
        let stateName = $"k{state}"
        if isInitial && isAccepting then
            sb.AppendLine($"  {stateName} [isInitial=true, isAccepting=true];") |> ignore
        elif isInitial then
            sb.AppendLine($"  {stateName} [isInitial=true];") |> ignore
        elif isAccepting then
            sb.AppendLine($"  {stateName} [isAccepting=true];") |> ignore
        else
            sb.AppendLine($"  {stateName};") |> ignore
    
    // Helper to escape special characters for DOT format
    let escapeDotLabel (c: char) : string =
        match c with
        | '"' -> "\\\""
        | '\\' -> "\\\\"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | ' ' -> "' '"
        | _ -> c.ToString()
    
    for KeyValue((state, label), nextStates) in nfa.transitions do
        let labelStr = escapeDotLabel label
        for nextState in nextStates do
            let fromName = $"k{state}"
            let toName = $"k{nextState}"
            sb.AppendLine($"  {fromName} -> {toName} [label=\"{labelStr}\"];") |> ignore
    
    sb.AppendLine("}") |> ignore
    sb.ToString()

let analysis (input: Input) : Output =
    RegexToEnfa.resetStateCounter ()
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString input.regex
    try
        let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
        let (enfa, _, _) = RegexToEnfa.regexToNFA ast
        let nfa = enfa_to_nfa enfa
        let pruned_nfa = prune_nfa nfa
        let dot = nfa_to_dot pruned_nfa
        { dot = dot }
    with
    | e -> 
        let errorDot = sprintf "digraph ERROR { label=\"Parse error: %s\"; }" e.Message
        { dot = errorDot }



