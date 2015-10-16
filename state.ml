open Ast

(* TODO placeholders *)
type value = int
type assumption_set = int

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

module Clock : CLOCK = MapClock

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

module Mem : MEM = MapMem

(* thread input configuration *)
type thread_input_config = {
  c : cmd;
  t : Clock.t;
  m : Mem.t;
  a : assumption_set;
}

(* thread output configuration *)
type thread_output_config = {
  c : cmd;
  m : Mem.t;
  a : assumption_set;
}

module ThreadOutputConfigSet =
  Set.Make(struct type t = thread_output_config let compare = compare end)

(* thread pool *)
module type THREAD_POOL =
  sig
    type t
    val initial : cmd -> t
    val update : tid -> cmd*Clock.t -> t -> t
    val lookup : tid -> t -> cmd*Clock.t
  end

module MapThreadPool : THREAD_POOL =
  struct
    type t = (cmd*Clock.t) TidMap.t
    let initial c = TidMap.add 0 (c,Clock.inc 0 Clock.bot) TidMap.empty
    let update id (c,time) t = TidMap.add id (c,time) t
    let lookup id t = TidMap.find id t
  end

module ThreadPool : THREAD_POOL = MapThreadPool

(* lock state *)
module type LOCK_STATE =
  sig
    type t
    val initial : t
    val update : var -> tid option*int*Clock.t -> t -> t
    val lookup : var -> t -> tid option*int*Clock.t
  end

module MapLockState : LOCK_STATE =
  struct
    type t = (tid option*int*Clock.t) VarMap.t
    let initial = VarMap.empty
    let update x (idopt,count,time) t =
      VarMap.add x (idopt,count,time) t
    let lookup x t =
      try VarMap.find x t with Not_found -> (None,0,Clock.bot)
  end

module LockState = MapLockState

(* thread pool configuration *)
type thread_pool_config = {
  tp : ThreadPool.t;
  m : Mem.t;
  ls : LockState.t;
  a : assumption_set;
}


module ThreadPoolConfigSet =
  Set.Make(struct type t = thread_pool_config let compare = compare end)
