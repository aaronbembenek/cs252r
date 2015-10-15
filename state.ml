open Ast

(* TODO place holder *)
type value = int

(* thread identifiers *)
type tid = int

(* useful maps *)
module TidMap = Map.Make(struct type t = tid let compare = compare end)
module VarMap = Map.Make(struct type t = var let compare = compare end)

(* vector clocks *)
module type CLOCK =
  sig
    type t 
    val bot : t 
    val inc : tid -> t -> t 
    val join : t -> t -> t 
  end

module MapClock : CLOCK =
  struct
    type t = int TidMap.t
    let bot = TidMap.empty
    let lookup (id : tid) (t : t) : int =
      try TidMap.find id t with Not_found -> 0 
    let inc (id : tid) (t : t) : t =
      TidMap.add id (lookup id t + 1) t
    let join : t -> t -> t =
      TidMap.fold (fun id time a -> TidMap.add id (max time (lookup id a)) a)
  end

module Clock = MapClock 

(* memory *)
module type MEM =
  sig
    type t
    val empty : t
    val lookup : var -> t -> (value*Clock.t) list
    val write : var -> value -> Clock.t -> t -> t
  end

module MapMem : MEM =
  struct
    type t = (value*Clock.t) list VarMap.t
    let empty = VarMap.empty
    let lookup (x : var) (m : t) : (value*Clock.t) list =
      try VarMap.find x m with Not_found -> [(0,Clock.bot)]
    (* TODO currently stores memory in newest-write-first order... okay? *)
    let write (x : var) (v : value) (t : Clock.t) (m : t) : t =
      let old = lookup x m in VarMap.add x ((v,t)::old) m
  end

module Mem = MapMem

(* Assumption set *)

(* Thread level input config - record *)

(* Thread level output config - record *)

(* Output config set? *)

(* Thread pool *)

(* Lock state *)

(* SymConfig - record *)

(* SymExec - queue??? *)
