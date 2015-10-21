open Ast
open Assumptions
open Mmodel
open State

exception TODO
exception Runtime_exception of string 

let eval_conc_binop (i1:int) (i2:int) (b:binop) : int =
  match b with
    Add -> i1 + i2
  | Sub -> i1 - i2
  | Mul -> i1 * i2
  | Div -> i1 / i2
  | _ -> let result = (match b with
      Eq -> i1 == i2
    | Neq -> i1 != i2
    | Lt -> i1 < i2
    | Lte -> i1 <= i2
    | Gt -> i1 > i2
    | Gte -> i1 >= i2
    | And -> (i1 != 0) && (i2 != 0)
    | Or -> (i1 != 0) || (i2 != 0)
    | _ -> assert false (* should never be reached *)) in
    if result then 1 else 0

let rec step_exp ({e; time; m; asmp}:exp_input_config) : Exp_output_config_set.t =
  match e with
  | Val _ -> assert false (* should never be reached *)
  | Var x ->
      Value_set.fold (fun v s -> Exp_output_config_set.add {e=Val(v); m; asmp} s)
        (Mem_model.read x time m) Exp_output_config_set.empty
  | Binop (e1,b,e2) ->
      match e1,e2 with
      (* first case: both exps are values, so compute binop *)
      | Val v1, Val v2 -> 
          (match v1, v2 with
          (* both values are concrete *)
          | Conc i1, Conc i2 ->
              Exp_output_config_set.singleton {e=Val(Conc(eval_conc_binop i1 i2 b)); m; asmp}
          (* at least one value is symbolic *)
          | _ -> let (new_sym, new_symbols, new_assumptions) = 
                add_binop_assumption v1 v2 asmp.symbols asmp.assumptions b in
              let new_assumption_set = {symbols = new_symbols; assumptions = new_assumptions} in
              Exp_output_config_set.singleton {e=Val(new_sym); m=m; asmp=new_assumption_set}
          )

      (* second case: first exp is a value but second is not, so take step with second *)
      | Val _, _ ->
          let oconfigs = step_exp {e=e2; time; m; asmp} in
          Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Exp_output_config_set.add {e=Binop(e1,b,e'); m=m'; asmp=asmp'} s)
            oconfigs Exp_output_config_set.empty 

      (* third case: first exp is not a value, so take step with first *)
      | _ ->
          let oconfigs = step_exp {e=e1; time; m; asmp} in
          Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Exp_output_config_set.add {e=Binop(e',b,e2); m=m'; asmp=asmp'} s)
            oconfigs Exp_output_config_set.empty 

(******************************************************************************)

let rec step_thread (s:thread_input_config) : Thread_output_config_set.t*annotation =
  match s.c with
  | Skip -> assert false (* should never be reached *)

  | Assign (x,e) ->
      (match e with
      | Val (Sym v) -> raise TODO (*DISCUSS*)
      | Val (Conc v) -> let new_mem = Mem.write x (Conc v) s.time s.m in
          Thread_output_config_set.singleton {c=Skip; m=new_mem; asmp=s.asmp}, Eps
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Thread_output_config_set.add {c=Assign(x,e'); m=m'; asmp=asmp'} s)
            oconfigs Thread_output_config_set.empty), Eps)

  | Seq (c1,c2) ->
      (match c1 with
      | Skip ->
          Thread_output_config_set.singleton {c=c2; m=s.m; asmp=s.asmp}, Eps
      | _ ->
          let oconfigs, anno = step_thread {c=c1; time=s.time; m=s.m; asmp=s.asmp} in
          (Thread_output_config_set.fold
            (fun {c=c'; m=m'; asmp=asmp'} s ->
              Thread_output_config_set.add {c=Seq(c',c2); m=m'; asmp=asmp'} s)
            oconfigs Thread_output_config_set.empty), anno)

  | If (e,c1,c2) ->
      (match e with
      | Val (Sym x) -> 
          (* x is true *)
          let (val_true, sym_true, asmp_true) = 
            add_binop_assumption (Sym x) (Conc 1) s.asmp.symbols s.asmp.assumptions Eq in
          let true_set = if check asmp_true then Thread_output_config_set.singleton 
              {c=c1; m=s.m; asmp={symbols=sym_true; assumptions=asmp_true}}
            else Thread_output_config_set.empty in
          (* x is false *)
          let (val_false, sym_false, asmp_false) = 
            add_binop_assumption (Sym x) (Conc 0) s.asmp.symbols s.asmp.assumptions Eq in
          let false_set = if check asmp_false then Thread_output_config_set.singleton 
              {c=c2; m=s.m; asmp={symbols=sym_false; assumptions=asmp_false}}
            else Thread_output_config_set.empty in
          (Thread_output_config_set.union true_set false_set, Eps)

      | Val (Conc x) -> let cnext = if x != 0 then c1 else c2 in
          Thread_output_config_set.singleton {c=cnext; m=s.m; asmp=s.asmp}, Eps
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Thread_output_config_set.add {c=If(e',c1,c2); m=m'; asmp=asmp'} s)
            oconfigs Thread_output_config_set.empty), Eps)

  | While (e,c') ->
      (Thread_output_config_set.singleton
        {c=If(e,Seq(c',While(e,c')),Skip); m=s.m; asmp=s.asmp}), Eps
        
  | Fork (x,c') ->
      let id = Thread_pool.new_id () in
      let m' = Mem.write x (Conc id) s.time s.m in
      Thread_output_config_set.singleton
        {c=Skip; m=m'; asmp=s.asmp}, (Fork(id,c')) 

  | Join e ->
      (match e with
      | Val (Conc n) ->
          Thread_output_config_set.singleton {c=Skip; m=s.m; asmp=s.asmp}, Join n
      | Val _ -> raise (Runtime_exception "cannot join over a symbolic value")
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Thread_output_config_set.add {c=Join(e'); m=m'; asmp=asmp'} s)
            oconfigs Thread_output_config_set.empty), Eps)

  | Lock x ->
      Thread_output_config_set.singleton {c=Skip; m=s.m; asmp=s.asmp}, Lock x

  | Unlock x ->
      Thread_output_config_set.singleton {c=Skip; m=s.m; asmp=s.asmp}, Unlock x

  | Symbolic x -> raise TODO (*DISCUSS*)

  | Assert e ->
      (match e with
        Val (Conc x) -> if x == 0 then assert false
          else (Thread_output_config_set.singleton {c=Skip; m=s.m; asmp=s.asmp}, Eps)
      | Val (Sym x) -> let (_, _, asmp) = 
          add_binop_assumption (Sym x) (Conc 0) s.asmp.symbols s.asmp.assumptions Neq in
        if (not (check asmp)) then assert false
          else (Thread_output_config_set.singleton {c=Skip; m=s.m; asmp=s.asmp}, Eps)

      (* TODO we might need to add another annotation that tells the thread pool
       * if an assertion fails *)
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              Thread_output_config_set.add {c=Assert(e');m=m';asmp=asmp'} s)
            oconfigs Thread_output_config_set.empty), Eps)

  | Return e -> assert false (* TODO do we still need return? *)

(******************************************************************************)

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
              raise (Runtime_exception ("cannot unlock unheld lock "^x))
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

(******************************************************************************)

let step_sym_exec (s:Thread_pool_config_set.t) : Thread_pool_config_set.t =
  let e = Thread_pool_config_set.choose s in
  let new_states = step_thread_pool e in
  let s' = Thread_pool_config_set.remove e s
  in Thread_pool_config_set.union s' new_states

(******************************************************************************)

let run (p:program) =
  let initial_state =
    let id = Thread_pool.new_id () in
    let tp = Thread_pool.update id (p,Clock.bot) Thread_pool.initial in
    let config = {tp=tp; m=Mem.empty; ls=Lock_state.initial;
                  asmp={symbols=TermMap.empty; assumptions=[]}} in
    Thread_pool_config_set.singleton config in
  let rec loop (s:Thread_pool_config_set.t) =
    if not (Thread_pool_config_set.is_empty s)
    then loop (step_sym_exec s) in
  loop initial_state

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
  run prog
