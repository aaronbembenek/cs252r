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

module Weak_mem_model : MEM_MODEL =
  struct
    let read x time m =
      let writes = Mem.lookup x m in
      let rec loop writes lookedat acc =
        match writes with
        | [] -> acc 
        | (v,c)::tl ->
            let acc' =
              if List.exists
                (fun (_,c') -> Clock.lte c c' && Clock.lte c' time) lookedat
              then acc else Value_set.add v acc in
            loop tl ((v,c)::lookedat) acc' in
      loop writes [] Value_set.empty
  end

module Mem_model = Weak_mem_model 
