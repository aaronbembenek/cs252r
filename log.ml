open Assumptions
open Ast
open State
open Yojson.Basic

(* Provides utilities to log special events in JSON format. *)

type event =
  | Assert_fail of pos 
  | Div_by_zero of pos 

let event_to_json (e:event) : string*json =
  let lineno pos = ("line", `Int pos) in
  match e with
  | Assert_fail pos -> "failed assertion", `Assoc [(lineno pos)]
  | Div_by_zero pos -> "division by zero", `Assoc [(lineno pos)]

let accum = ref []
  
let report (e:event) (m:Mem.t) (asmp:assumption_set) : unit =
  let e' = event_to_json e in
  if not (List.mem e' !accum) then
  accum := e'::(!accum)

let dump (out:out_channel) : unit =
  pretty_to_channel out (`Assoc (List.rev !accum));
  Printf.fprintf out "\n"
