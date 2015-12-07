open Ast
open State

(* Abstract a memory model as a single function `read` that returns all values
 * that can be read from a given memory location at a given time by a given
 * thread in a given memory state.
 *
 * NOTE: in our formal semantics, `read` does not take the thread identifier as
 * an argument. We modified the type of `read` here to make it easy to
 * implement the restricted PJMM. *)
module type MEM_MODEL =
  sig
    val read : var -> Clock.t -> tid -> Mem.t -> Value_set.t
  end

(* The strictly consistent memory model: a thread is guaranteed to read the
 * most recent write to a location by any thread. *)
module Strictly_consistent_mmodel : MEM_MODEL =
  struct
    let read x time tid m = 
      let (v,_,_) = List.hd (Mem.lookup x m) in
      Value_set.singleton v 
  end

(* The Progressive Java Memory Model: a thread at time t can read a value
 * written at time t' if t does not happen before t' and there is not a write at
 * time t'' such that t' happens before t'' and t'' happens before t. *)
module Pjmm : MEM_MODEL =
  struct
    let read x time tid m =
      let writes = Mem.lookup x m in
      let rec loop writes lookedat acc =
        match writes with
        | [] -> acc 
        | (v,c,t)::tl ->
            let acc' =
              if List.exists
                (fun (_,c',_) -> Clock.lte c c' && Clock.lte c' time) lookedat
              then acc else Value_set.add v acc in
            loop tl ((v,c,t)::lookedat) acc' in
      loop writes [] Value_set.empty
  end

(* The restricted PJMM: a stricter version of the PJMM that guarantees that a
 * thread will only read values from a memory location that were written after
 * its most recent write to that location (including its most recent write). *)
module Rpjmm : MEM_MODEL =
  struct
    let read x time tid m =
      (* Do not consider writes that occured before the last write to this
       * memory location by the thread `tid`. *)
      let writes =
        let rec loop l acc =
          match l with
          | [] -> acc
          | (v,c,t)::tl ->
              if t = tid then (v,c,t)::acc
              else loop tl ((v,c,t)::acc) in
        loop (Mem.lookup x m) [] in
      let rec loop writes lookedat acc =
        match writes with
        | [] -> acc 
        | (v,c,t)::tl ->
            let acc' =
              if List.exists
                (fun (_,c',_) -> Clock.lte c c' && Clock.lte c' time) lookedat
              then acc else Value_set.add v acc in
            loop tl ((v,c,t)::lookedat) acc' in
      loop (List.rev writes) [] Value_set.empty
  end

module Mem_model =
#ifdef STRICT 
  Strictly_consistent_mmodel
#elif defined RPJMM 
  Rpjmm 
#elif defined PJMM 
  Pjmm
#else
#error "Must specify memory model to use at compile time."
#endif
