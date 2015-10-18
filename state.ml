open Ast

(* TODO placeholders *)
type assumption_set = int

(* thread identifiers *)
type tid = int

(* useful maps *)
module TidMap = Map.Make(struct type t = tid let compare = compare end)
module VarMap = Map.Make(struct type t = var let compare = compare end)

(*****************************************************************************
 * VECTOR CLOCKS
 *****************************************************************************)

module type CLOCK =
  sig
    type t 
    val bot : t 
    val inc : tid -> t -> t 
    val join : t -> t -> t 
  end

module Map_clock : CLOCK =
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

module Clock : CLOCK = Map_clock

(*****************************************************************************
 * MEMORY
 *****************************************************************************)

module type MEM =
  sig
    type t
    val empty : t
    val lookup : var -> t -> (value*Clock.t) list
    val write : var -> value -> Clock.t -> t -> t
  end

module Map_mem : MEM =
  struct
    type t = (value*Clock.t) list VarMap.t
    let empty = VarMap.empty
    let lookup (x : var) (m : t) : (value*Clock.t) list =
      try VarMap.find x m with Not_found -> [(Conc 0,Clock.bot)]
    (* TODO currently stores memory in newest-write-first order... okay? *)
    let write (x : var) (v : value) (t : Clock.t) (m : t) : t =
      let old = lookup x m in VarMap.add x ((v,t)::old) m
  end

module Mem : MEM = Map_mem

(******************************************************************************
 * THREAD-LEVEL STATE 
 ******************************************************************************)

(* thread input configuration *)
type thread_input_config = {
  c    : cmd;
  time : Clock.t;
  m    : Mem.t;
  asmp : assumption_set;
}

(* thread output configuration *)
type thread_output_config = {
  c    : cmd;
  m    : Mem.t;
  asmp : assumption_set;
}

(* TODO probably need to switch the backing of this type *)
module Thread_output_config_set =
  Set.Make(struct type t = thread_output_config let compare = compare end)

(* annotations are used to pass information relevant to thread pool-level state
 * with output configuration set *)
type annotation = | Eps | Fork of int*cmd | Join of int
                  | Lock of var | Unlock of var

(******************************************************************************
 * THREAD POOL-LEVEL STATE
 ******************************************************************************)

(* thread pool *)
module type THREAD_POOL =
  sig
    type t
    val initial : t
    val update : tid -> cmd*Clock.t -> t -> t
    val lookup : tid -> t -> cmd*Clock.t
    val new_id : unit -> tid
    val choose : t -> tid*(cmd*Clock.t)
    val is_empty : t -> bool
    val remove : tid -> t -> t
  end

module Map_thread_pool : THREAD_POOL =
  struct
    type t = (cmd*Clock.t) TidMap.t
    let initial = TidMap.empty
    let update id (c,time) t = TidMap.add id (c,time) t
    let lookup id t =
        try TidMap.find id t with Not_found -> (Skip,Clock.bot)
    let cur_id = ref 0
    let new_id () = let id = !cur_id in cur_id := !cur_id + 1; id
    let choose t =
      let bindings = TidMap.bindings t in
      let i = Random.int (List.length bindings) in
      List.nth bindings i
    let is_empty = TidMap.is_empty
    let remove = TidMap.remove
  end

module Thread_pool : THREAD_POOL = Map_thread_pool

(* lock state *)
module type LOCK_STATE =
  sig
    type t
    val initial : t
    val update : var -> tid option*int*Clock.t -> t -> t
    val lookup : var -> t -> tid option*int*Clock.t
  end

module Map_lock_state : LOCK_STATE =
  struct
    type t = (tid option*int*Clock.t) VarMap.t
    let initial = VarMap.empty
    let update x (idopt,count,time) t =
      VarMap.add x (idopt,count,time) t
    let lookup x t =
      try VarMap.find x t with Not_found -> (None,0,Clock.bot)
  end

module Lock_state : LOCK_STATE = Map_lock_state

(* thread pool configuration *)
type thread_pool_config = {
  tp   : Thread_pool.t;
  m    : Mem.t;
  ls   : Lock_state.t;
  asmp : assumption_set;
}

(* TODO probably need to switch the backing of this type *)
module Thread_pool_config_set =
  Set.Make(struct type t = thread_pool_config let compare = compare end)
