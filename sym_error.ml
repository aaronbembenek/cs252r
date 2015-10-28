open Ast
open State
open Yojson.Basic

type error =
  | Assert of exp 
  | Div_by_zero of exp

let error_to_json (e:error) : string*json =
  match e with
  | Assert exp -> "failed assertion", `String (Prettyprint.pp_exp exp)
  | Div_by_zero exp -> "division by zero", `String (Prettyprint.pp_exp exp)

let accum = ref []

let report (e:error) (m:Mem.t) (asmp:assumption_set) : unit =
  accum := (error_to_json e)::(!accum)

let dump (out:out_channel) : unit =
  pretty_to_channel out (`Assoc (!accum))
