open Ast
open State

exception TODO
exception RuntimeException of string 

let rec eval_exp (e:exp) (m:Mem.t) : value =
  match e with
    Val (Conc i) -> Conc i
  | Val (Sym x) -> Sym x
  | Var x -> raise TODO
  | Binop(e1,b,e2) ->
      let (v1,v2) = (eval_exp e1 m, eval_exp e2 m) in
      match v1,v2 with
      | Conc i1, Conc i2 ->
          let v = match b with
              Add -> i1 + i2
            | Sub -> i1 - i2
            | Mul -> i1 * i2
            | Div -> i1 / i2
            | Eq -> if i1 == i2 then 1 else 0
            | Neq -> if i1 != i2 then 1 else 0
            | Lt -> if i1 < i2 then 1 else 0
            | Lte -> if i1 <= i2 then 1 else 0
            | Gt -> if i1 > i2 then 1 else 0
            | Gte -> if i1 >= i2 then 1 else 0
            | And -> if (i1 != 0) && (i2 != 0) then 1 else 0
            | Or -> if (i1 != 0) || (i2 != 0) then 1 else 0 in
          Conc v
      | _ -> raise TODO

let step_thread (s:thread_input_config) : Thread_output_config_set.t*annotation =
  match s.c with
  | Skip -> assert false (* should never be reached *)
  | Assign (x,e) -> raise TODO 
  | Seq (c1,c2) -> raise TODO
  | If (b,c1,c2) -> raise TODO
  | While (b,c') -> raise TODO
  | Fork (x,c') -> raise TODO 
  | Join e -> raise TODO
  | Lock x -> raise TODO
  | Unlock x -> raise TODO
  | Symbolic x -> raise TODO
  | Assert e -> raise TODO
  | Return e -> assert false (* TODO do we still need return? *)

let step_thread_pool (s:thread_pool_config) : Thread_pool_config_set.t =
  if Thread_pool.is_empty s.tp then Thread_pool_config_set.empty
  else
    let id,(c,time) = Thread_pool.choose s.tp in
    if c = Skip then
      let tp' = Thread_pool.remove id s.tp in
      Thread_pool_config_set.singleton {tp=tp'; m=s.m; ls=s.ls; asmp=s.asmp}
    else
      let iconfig = {c=c; time=time; m=s.m; asmp=s.asmp} in
      let oconfigs,anno = step_thread iconfig in
      match anno with
      | Fork (id',c') ->
          assert (Thread_output_config_set.cardinal oconfigs = 1);
          let oconfig = Thread_output_config_set.choose oconfigs in
          assert (oconfig.asmp = s.asmp);
          let time_for_id = Clock.inc id time in
          let time_for_id' = Clock.inc id' time in
          let tp' = Thread_pool.update id' (c',time_for_id')
            (Thread_pool.update id (oconfig.c,time_for_id) s.tp) in
          let r = {tp=tp'; m=oconfig.m; ls=s.ls; asmp=oconfig.asmp} in
          Thread_pool_config_set.singleton r

      | Join id' ->
          assert (Thread_output_config_set.cardinal oconfigs = 1);
          let oconfig = Thread_output_config_set.choose oconfigs in
          assert (oconfig.asmp = s.asmp);
          assert (oconfig.m = iconfig.m);
          let (c',time') = Thread_pool.lookup id' s.tp in
          let r =
            match c' with
            | Skip ->
                let tp' = Thread_pool.update id (oconfig.c,Clock.join time time') s.tp in
                {tp=tp'; m=oconfig.m; ls=s.ls; asmp=oconfig.asmp }
            | _ -> s in (* can't progress *)
          Thread_pool_config_set.singleton r
          
      | Lock x ->
          assert (Thread_output_config_set.cardinal oconfigs = 1);
          let oconfig = Thread_output_config_set.choose oconfigs in
          assert (oconfig.asmp = s.asmp);
          assert (oconfig.m = iconfig.m);
          let owner,cnt,time' = Lock_state.lookup x s.ls in
          let id' = match owner with None -> id | Some id' -> id' in
          let r = if id = id' then
            let tp' = Thread_pool.update id (oconfig.c,Clock.join time time') s.tp in
            let ls' = Lock_state.update x (Some id,cnt + 1,time') s.ls in
            {tp=tp'; m=oconfig.m; ls=ls'; asmp=oconfig.asmp}
          else
            s in (* can't progress *)
          Thread_pool_config_set.singleton r

      | Unlock x ->
          assert (Thread_output_config_set.cardinal oconfigs = 1);
          let oconfig = Thread_output_config_set.choose oconfigs in
          assert (oconfig.asmp = s.asmp);
          assert (oconfig.m = iconfig.m);
          let owner,cnt,_ = Lock_state.lookup x s.ls in
          let id' = match owner with None -> id + 1 | Some id' -> id' in
          let ls' =
            if id <> id' then
              raise (RuntimeException ("cannot unlock unheld lock "^x))
            else
              let cnt' = cnt - 1 in
              let owner' = if cnt' = 0 then None else Some id in
              Lock_state.update x (owner',cnt',time) s.ls in
          let tp' = Thread_pool.update id (oconfig.c,Clock.inc id time) s.tp in
          let r = {tp=tp'; m=oconfig.m; ls=ls'; asmp=oconfig.asmp} in
          Thread_pool_config_set.singleton r

      | Eps ->
          let time' = Clock.inc id time in
          Thread_output_config_set.fold
            (fun {c=c'; m=m'; asmp=asmp'} a ->
              let tp' = Thread_pool.update id (c',time') s.tp in
              Thread_pool_config_set.add {tp=tp'; m=m'; ls=s.ls; asmp=asmp'} a)
            oconfigs Thread_pool_config_set.empty

let step_sym_exec (s:Thread_pool_config_set.t) : Thread_pool_config_set.t =
  let e = Thread_pool_config_set.choose s in
  let new_states = step_thread_pool e in
  let s' = Thread_pool_config_set.remove e s
  in Thread_pool_config_set.union s' new_states

let initial_state (p:program) =
  let id = Thread_pool.new_id () in
  let tp = Thread_pool.update id (p,Clock.bot) Thread_pool.initial in
  let config = {tp=tp; m=Mem.empty; ls=Lock_state.initial; asmp=0} in
  Thread_pool_config_set.singleton config

let run (p:program) =
  let initial_state =
    let id = Thread_pool.new_id () in
    let tp = Thread_pool.update id (p,Clock.bot) Thread_pool.initial in
    let config = {tp=tp; m=Mem.empty; ls=Lock_state.initial; asmp=0} in
    Thread_pool_config_set.singleton config in
  let rec loop (s:Thread_pool_config_set.t) =
    if not (Thread_pool_config_set.is_empty s)
    then loop (step_sym_exec s) in
  loop initial_state

(*
let run (p : program) : int =
  let rec loop () =
    match get_active_tids () with
    | [] -> 0
    | tids -> List.iter step_thread tids; loop () in
  add_thread (create_new_tid ()) p;
  try loop () with Done n -> n

(* Copied directly from CS 153 material. *)
let parse_file () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-evaluate]\n");
        exit 1) in
      let ch = open_in argv.(1) in
        Parse.program Lex.lexer (Lexing.from_channel ch)

let _ =
  let prog = parse_file () in
  let return_val = (run prog) in
    (* Print result for testing. *)
    print_int return_val;
    print_string "\n";
    (* Exit with return value. *)
    exit return_val
*)
