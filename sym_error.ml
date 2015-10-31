open Ast
open State
open Yojson.Basic
open Assumptions

type error =
  | Assert of exp 
  | Div_by_zero of exp

let error_to_json (e:error) : string*json =
  match e with
  | Assert exp -> "failed assertion", `String (Prettyprint.pp_exp exp)
  | Div_by_zero exp -> "division by zero", `String (Prettyprint.pp_exp exp)

let accum = ref []

let is_div_by_zero (v1:value) (b:binop) (v2:value) (syms:termMap) (assumps:assumptions) : bool =
	let error = (match b, v2 with
		Div, Conc x -> x == 0
	| Div, Sym x -> check (add_if_assumption (Sym x) false syms assumps)
	| _ -> false) in
	let e = (Binop(Val v1, b, Val v2)) in
	if error then (accum := (error_to_json (Div_by_zero e))::(!accum); true) else false

let report (e:error) (m:Mem.t) (asmp:assumption_set) : unit =
  accum := (error_to_json e)::(!accum)

let dump (out:out_channel) : unit =
  pretty_to_channel out (`Assoc (!accum))
