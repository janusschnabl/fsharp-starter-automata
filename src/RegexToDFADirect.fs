module RegexToDfaDirect
open Io.ce_regex_to_dfa_direct
open RegexAST
open RegexToEnfa

/// Represents a DFA state (set of ENFA states)
type DFAState = Set<int>

/// Represents a DFA
type DFA = {
    states: Set<DFAState>
    startState: DFAState
    acceptStates: Set<DFAState>
    transitions: Map<(DFAState * char), DFAState>
    alphabet: Set<char>
}

/// Compute epsilon closure for a single state in ENFA
let rec epsilon_closure (enfa: EpsilonNFA) (state: int) : Set<int> =
    let visited = ref (Set.empty)
    let worklist = ref [state]
    
    while !worklist <> [] do
        let current = (!worklist).Head
        worklist := (!worklist).Tail
        
        if not (Set.contains current !visited) then
            visited := Set.add current !visited
            
            match Map.tryFind (current, None) enfa.transitions with
            | Some targets ->
                for target in targets do
                    if not (Set.contains target !visited) then
                        worklist := target :: !worklist
            | None -> ()
    
    !visited

/// Compute epsilon closure for a set of states
let epsilon_closure_set (enfa: EpsilonNFA) (states: Set<int>) : Set<int> =
    Set.fold (fun acc state ->
        Set.union acc (epsilon_closure enfa state)
    ) Set.empty states

/// Get alphabet from ENFA (only non-epsilon symbols)
let get_alphabet (enfa: EpsilonNFA) : Set<char> =
    Map.fold (fun acc (_, symbol: char option) _targets ->
        match symbol with
        | Some c -> Set.add c acc
        | None -> acc
    ) Set.empty enfa.transitions

/// Compute next DFA state from current DFA state via symbol (directly from ENFA)
let next_dfa_state (enfa: EpsilonNFA) (state_set: DFAState) (symbol: char) : DFAState =
    let mutable targets = Set.empty
    
    for state in state_set do
        match Map.tryFind (state, Some symbol) enfa.transitions with
        | Some transition_targets ->
            for target in transition_targets do
                targets <- Set.add target targets
        | None -> ()
    
    epsilon_closure_set enfa targets

/// Check if a DFA state is accepting
let is_accepting (dfa_state: DFAState) (enfa: EpsilonNFA) : bool =
    Set.exists (fun state -> Set.contains state enfa.acceptStates) dfa_state

/// Convert ENFA to DFA directly using subset construction
let enfa_to_dfa (enfa: EpsilonNFA) : DFA =
    let alphabet = get_alphabet enfa
    let start_dfa_state = epsilon_closure_set enfa (Set.singleton enfa.startState)
    
    let dfa_states = ref Map.empty
    let work_to_do = ref [start_dfa_state]
    
    while !work_to_do <> [] do
        let current_subset = (!work_to_do).Head
        work_to_do := (!work_to_do).Tail
        
        if not (Map.containsKey current_subset !dfa_states) then
            let transitions = ref Map.empty
            
            // Process each symbol in the alphabet
            for symbol in alphabet do
                let next_states = next_dfa_state enfa current_subset symbol
                
                // Always add transition, even if empty
                transitions := Map.add (current_subset, symbol) next_states !transitions
                
                // Always add to work queue
                work_to_do := next_states :: !work_to_do
            
            dfa_states := Map.add current_subset !transitions !dfa_states
    
    // Collect all unique DFA states
    let all_states = Map.fold (fun acc state _ -> Set.add state acc) Set.empty !dfa_states
    
    // Build flat transitions map
    let transitions = ref Map.empty
    Map.iter (fun state trans_map ->
        Map.iter (fun (state_sym_pair: DFAState * char) (next_state: DFAState) ->
            transitions := Map.add state_sym_pair next_state !transitions
        ) trans_map
    ) !dfa_states
    
    // Determine accepting states
    let accept_states = 
        Set.filter (fun dfa_state -> is_accepting dfa_state enfa) all_states
    
    {
        states = all_states
        startState = start_dfa_state
        acceptStates = accept_states
        transitions = !transitions
        alphabet = alphabet
    }

/// Convert DFA to DOT format
let dfa_to_dot (dfa: DFA) : string =
    let sb = System.Text.StringBuilder()
    sb.AppendLine("digraph DFA {") |> ignore
    sb.AppendLine("  rankdir=LR;") |> ignore
    
    // Create state name mapping for cleaner output
    let state_names = ref Map.empty
    let mutable state_idx = 0
    
    for state in dfa.states do
        state_names := Map.add state state_idx !state_names
        state_idx <- state_idx + 1
    
    // Add nodes
    for state in dfa.states do
        let idx = Map.find state !state_names
        let isInitial = state = dfa.startState
        let isAccepting = Set.contains state dfa.acceptStates
        let stateName = $"q{idx}"
        
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
    
    // Add transitions
    Map.iter (fun (from_state: DFAState, symbol: char) (to_state: DFAState) ->
        let from_idx = Map.find from_state !state_names
        let to_idx = Map.find to_state !state_names
        let fromName = $"q{from_idx}"
        let toName = $"q{to_idx}"
        let labelStr = escapeDotLabel symbol
        sb.AppendLine($"  {fromName} -> {toName} [label=\"{labelStr}\"];") |> ignore
    ) dfa.transitions
    
    sb.AppendLine("}") |> ignore
    sb.ToString()

let analysis (input: Input) : Output =
    RegexToEnfa.resetStateCounter ()
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString input.regex
    try
        let ast = RegexGrammar.regex RegexLexer.tokenize lexbuf
        let (enfa, _, _) = RegexToEnfa.regexToNFA ast
        let dfa = enfa_to_dfa enfa
        let dot = dfa_to_dot dfa
        { dot = dot }
    with
    | e -> 
        let errorDot = sprintf "digraph ERROR { label=\"Parse error: %s\"; }" e.Message
        { dot = errorDot }
