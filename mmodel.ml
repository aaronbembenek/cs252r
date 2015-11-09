open Ast
open State

module type MEM_MODEL =
  sig
    val read : var -> Clock.t -> tid -> Mem.t -> Value_set.t
  end

module Sequential_mem_model : MEM_MODEL =
  struct
    let read x time tid m = 
      let (v,_,_) = List.hd (Mem.lookup x m) in
      Value_set.singleton v 
  end

module Weaker_mem_model : MEM_MODEL =
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

module Weak_mem_model : MEM_MODEL =
  struct
    let read x time tid m =
      (*
      List.iter (fun (v,_,_) -> print_string (Prettyprint.pp_exp (Val v,0)^", ")) (Mem.lookup x m);
      print_newline ();
      *)
      let writes =
        let rec loop l acc =
          match l with
          | [] -> acc
          | (v,c,t)::tl ->
              if t = tid then (v,c,t)::acc
              else loop tl ((v,c,t)::acc) in
        loop (Mem.lookup x m) [] in
      (*
      List.iter (fun (v,_,_) -> print_string (Prettyprint.pp_exp (Val v,0)^", ")) writes;
      print_newline ();
      *)
      (* TODO this is slow... *)
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
#ifdef SEQ 
  Sequential_mem_model
#elif defined WEAK 
  Weak_mem_model
#elif defined WEAKER
  Weaker_mem_model
#else
#error "Must specify memory model to use at compile time."
#endif
