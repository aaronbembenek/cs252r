open Ast
open State

module type MEM_MODEL =
  sig
    val read : var -> Clock.t -> Mem.t -> Value_set.t
  end

module Sequential_mem_model : MEM_MODEL =
  struct
    let read x time m = Value_set.singleton (fst (List.hd (Mem.lookup x m)))
  end

module Mem_model = Sequential_mem_model
