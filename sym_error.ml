open Ast
open State

type error =
  | Assert of exp 
  | Div_by_zero of exp

let report (err:error) (m:Mem.t) (asmp:assumption_set) = ()
(* TODO: Add to JSON *)

let dump (out:out_channel) = ()
(* TODO: Dump error report to output channel *)
