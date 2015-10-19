open Ast

(* TODO placeholders *)
type assumption_set = int

(* thread identifiers *)
type tid = int

(* useful maps *)
module Tid_map = Map.Make(struct type t = tid let compare = compare end)
module Var_map = Map.Make(struct type t = var let compare = compare end)

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
    type t = int Tid_map.t
    let bot = Tid_map.empty
    let lookup (id : tid) (t : t) : int =
      try Tid_map.find id t with Not_found -> 0 
    let inc (id : tid) (t : t) : t =
      Tid_map.add id (lookup id t + 1) t
    let join : t -> t -> t =
      Tid_map.fold (fun id time a -> Tid_map.add id (max time (lookup id a)) a)
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
    type t = (value*Clock.t) list Var_map.t
    let empty = Var_map.empty
    let lookup (x : var) (m : t) : (value*Clock.t) list =
      try Var_map.find x m with Not_found -> [(Conc 0,Clock.bot)]
    (* TODO currently stores memory in newest-write-first order... okay? *)
    let write (x : var) (v : value) (t : Clock.t) (m : t) : t =
      let old = lookup x m in Var_map.add x ((v,t)::old) m
  end

module Mem : MEM = Map_mem

(******************************************************************************
 * EXPRESSION-LEVEL STATE 
 ******************************************************************************)

(* thread input configuration *)
type exp_input_config = {
  e    : exp;
  time : Clock.t;
  m    : Mem.t;
  asmp : assumption_set;
}

(* thread output configuration *)
type exp_output_config = {
  e    : exp;
  m    : Mem.t;
  asmp : assumption_set;
}

(* TODO probably need to switch the backing of this type *)
module Exp_output_config_set =
  Set.Make(struct type t = exp_output_config let compare = compare end)

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
    type t = (cmd*Clock.t) Tid_map.t
    let initial = Tid_map.empty
    let update id (c,time) t = Tid_map.add id (c,time) t
    let lookup id t =
        try Tid_map.find id t with Not_found -> (Skip,Clock.bot)
    let cur_id = ref 0
    let new_id () = let id = !cur_id in cur_id := !cur_id + 1; id
    let choose t =
      let bindings = Tid_map.bindings t in
      let i = Random.int (List.length bindings) in
      List.nth bindings i
    let is_empty = Tid_map.is_empty
    let remove = Tid_map.remove
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
    type t = (tid option*int*Clock.t) Var_map.t
    let initial = Var_map.empty
    let update x (idopt,count,time) t =
      Var_map.add x (idopt,count,time) t
    let lookup x t =
      try Var_map.find x t with Not_found -> (None,0,Clock.bot)
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
