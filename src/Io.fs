module rec Io

open System.Text.Json.Serialization

module BiGCL =
  type Input =
    { commands: string }
  type Output =
    { binary: string }

module Calculator =
  type Input =
    { expression: string }
  type Output =
    { result: string
      error: string }

module Compiler =
  type Input =
    { commands: string
      determinism: GCL.Determinism }
  type Output =
    { dot: string }

module GCL =
  [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.ExternalTag + JsonUnionEncoding.UnwrapFieldlessTags + JsonUnionEncoding.UnwrapSingleFieldCases)>]
  type Determinism =
    | Deterministic
    | NonDeterministic
  type TargetDef =
    { name: string
      kind: GCL.TargetKind }
  [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.ExternalTag + JsonUnionEncoding.UnwrapFieldlessTags + JsonUnionEncoding.UnwrapSingleFieldCases)>]
  type TargetKind =
    | Variable
    | Array
  type Variable = string
  type Array = string

module Interpreter =
  type Input =
    { commands: string
      determinism: GCL.Determinism
      assignment: Interpreter.InterpreterMemory
      trace_length: int32 }
  type Output =
    { initial_node: string
      final_node: string
      dot: string
      trace: List<Interpreter.Step>
      termination: Interpreter.TerminationState }
  type InterpreterMemory =
    { variables: Map<GCL.Variable, int32>
      arrays: Map<GCL.Array, List<int32>> }
  [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.ExternalTag + JsonUnionEncoding.UnwrapFieldlessTags + JsonUnionEncoding.UnwrapSingleFieldCases)>]
  type TerminationState =
    | Running
    | Stuck
    | Terminated
  type Step =
    { action: string
      node: string
      memory: Interpreter.InterpreterMemory }

module Parser =
  type Input =
    { commands: string }
  type Output =
    { pretty: string }

module RiscV =
  type Input =
    { commands: string }
  type Output =
    { assembly: string }
  type Annotation =
    { pc: uint32
      regs: Map<string, int32>
      variables: Map<string, int32 * int32>
      memory: List<int32> }

module SecurityAnalysis =
  type Input =
    { commands: string
      classification: Map<string, string>
      lattice: SecurityAnalysis.SecurityLatticeInput }
  type Output =
    { actual: List<SecurityAnalysis.Flow>
      allowed: List<SecurityAnalysis.Flow>
      violations: List<SecurityAnalysis.Flow>
      is_secure: bool }
  type Meta =
    { lattice: SecurityAnalysis.SecurityLattice
      targets: List<GCL.TargetDef> }
  type SecurityLatticeInput =
    { rules: List<SecurityAnalysis.Flow> }
  type SecurityLattice =
    { allowed: List<SecurityAnalysis.Flow> }
  type Flow =
    { from: string
      into: string }

module SignAnalysis =
  type Input =
    { commands: string
      determinism: GCL.Determinism
      assignment: SignAnalysis.SignMemory }
  type Output =
    { initial_node: string
      final_node: string
      nodes: Map<string, List<SignAnalysis.SignMemory>>
      dot: string }
  type SignMemory =
    { variables: Map<GCL.Variable, SignAnalysis.Sign>
      arrays: Map<GCL.Array, List<SignAnalysis.Sign>> }
  [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.ExternalTag + JsonUnionEncoding.UnwrapFieldlessTags + JsonUnionEncoding.UnwrapSingleFieldCases)>]
  type Sign =
    | Positive
    | Zero
    | Negative

module ce_regex_to_dfa =
  type Input =
    { regex: string }
  type Output =
    { dot: string }

module ce_regex_to_dfa_direct =
  type Input =
    { regex: string }
  type Output =
    { dot: string }

module ce_regex_to_enfa =
  type Input =
    { regex: string }
  type Output =
    { dot: string }

module ce_regex_to_nfa =
  type Input =
    { regex: string }
  type Output =
    { dot: string }

module ce_shell =
  [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.UnwrapSingleFieldCases, UnionTagName = "analysis", UnionFieldsName = "io")>]
  type Envs =
    | Calculator of input: Calculator.Input * output: Calculator.Output * meta: unit * annotation: unit
    | RegexToEnfa of input: ce_regex_to_enfa.Input * output: ce_regex_to_enfa.Output * meta: unit * annotation: unit
    | RegexToNfa of input: ce_regex_to_nfa.Input * output: ce_regex_to_nfa.Output * meta: unit * annotation: unit
    | RegexToDfa of input: ce_regex_to_dfa.Input * output: ce_regex_to_dfa.Output * meta: unit * annotation: unit
    | RegexToDfaDirect of input: ce_regex_to_dfa_direct.Input * output: ce_regex_to_dfa_direct.Output * meta: unit * annotation: unit
    | Compiler of input: Compiler.Input * output: Compiler.Output * meta: unit * annotation: unit
    | Interpreter of input: Interpreter.Input * output: Interpreter.Output * meta: List<GCL.TargetDef> * annotation: unit
    | BiGCL of input: BiGCL.Input * output: BiGCL.Output * meta: unit * annotation: unit
    | RiscV of input: RiscV.Input * output: RiscV.Output * meta: unit * annotation: RiscV.Annotation
    | Parser of input: Parser.Input * output: Parser.Output * meta: unit * annotation: unit
    | Security of input: SecurityAnalysis.Input * output: SecurityAnalysis.Output * meta: SecurityAnalysis.Meta * annotation: unit
    | Sign of input: SignAnalysis.Input * output: SignAnalysis.Output * meta: List<GCL.TargetDef> * annotation: unit

