open Ast

(* thread identifiers *)
type tid = int

(* vector clocks *)
module TidMap = Map.Make(struct type t = tid let compare = compare end)
type clock = int TidMap.t
let bot_clock = TidMap.empty
let lookup_time (id : tid) (t : clock) : int =
  try TidMap.find id t with Not_found -> 0 
let inc_clock (id : tid) (t : clock) : clock =
  TidMap.add id (lookup_time id t + 1) t
let join_clocks : clock -> clock -> clock =
  TidMap.fold (fun id time a -> TidMap.add id (max time (lookup_time id a)) a)

(* memory *)
module VarMap = Map.Make(struct type t = var let compare = compare end)
(* TODO change to val *)
type mem = (int*clock) list VarMap.t
let empty_mem = VarMap.empty
let lookup_mem (x : var) (m : mem) : (int*clock) list =
    try VarMap.find x m with Not_found -> [(0,bot_clock)]
(* TODO currently stores memory in newest write first order... okay? *)
let write_mem (x : var) (v : int) (t : clock) (m : mem) : mem =
    let old = lookup_mem x m in VarMap.add x ((v,t)::old) m

(* Assumption set *)

(* Thread level input config - record *)

(* Thread level output config - record *)

(* Output config set? *)

(* SymConfig - record *)

(* SymExec - set *)
