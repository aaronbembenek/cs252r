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
  (* TODO: check for divide by zero error *)
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
          (* TODO: check for divide by zero error *)
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

let rec step_thread (s:thread_input_config) : thread_output_config list*annotation =
  match s.c with
  | Skip -> assert false (* should never be reached *)

  | Assign (x,e) ->
      (match e with
      | Val v -> let new_mem = Mem.write x v s.time s.m in
          [{c=Skip; m=new_mem; asmp=s.asmp}], Eps
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              {c=Assign(x,e'); m=m'; asmp=asmp'}::s)
            oconfigs []), Eps)

  | Seq (c1,c2) ->
      (match c1 with
      | Skip -> [{c=c2; m=s.m; asmp=s.asmp}], Eps
      | _ ->
          let oconfigs, anno = step_thread {c=c1; time=s.time; m=s.m; asmp=s.asmp} in
          (List.fold_right
            (fun {c=c'; m=m'; asmp=asmp'} s ->
              {c=Seq(c',c2); m=m'; asmp=asmp'}::s)
            oconfigs []), anno)

  | If (e,c1,c2) ->
      (match e with
      | Val (Sym x) -> 
          (* x is true *)
          let asmp_true = add_if_assumption (Sym x) true s.asmp.symbols s.asmp.assumptions in
          let true_set =
            if check asmp_true then
              [{c=c1; m=s.m; asmp={symbols=s.asmp.symbols; assumptions=asmp_true}}]
            else [] in
          (* x is false *)
          let asmp_false = add_if_assumption (Sym x) false s.asmp.symbols s.asmp.assumptions in
          let false_set =
            if check asmp_false then
              [{c=c2; m=s.m; asmp={symbols=s.asmp.symbols; assumptions=asmp_false}}]
            else [] in
          (true_set @ false_set, Eps)

      | Val (Conc x) -> let cnext = if x != 0 then c1 else c2 in
          [{c=cnext; m=s.m; asmp=s.asmp}], Eps

      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              {c=If(e',c1,c2); m=m'; asmp=asmp'}::s)
            oconfigs []), Eps)

  | While (e,c') ->
      [{c=If(e,Seq(c',While(e,c')),Skip); m=s.m; asmp=s.asmp}], Eps
        
  | Fork (x,c') ->
      let id = Thread_pool.new_id () in
      let m' = Mem.write x (Conc id) s.time s.m in
      [{c=Skip; m=m'; asmp=s.asmp}], (Fork(id,c'))

  | Join e ->
      (match e with
      | Val (Conc n) -> [{c=Skip; m=s.m; asmp=s.asmp}], Join n
      | Val _ -> raise (Runtime_exception "cannot join over a symbolic value")
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              {c=Join(e'); m=m'; asmp=asmp'}::s)
            oconfigs []), Eps)

  | Lock x -> [{c=Skip; m=s.m; asmp=s.asmp}], Lock x

  | Unlock x -> [{c=Skip; m=s.m; asmp=s.asmp}], Unlock x

  | Symbolic x -> (let (new_sym, new_symbols) = make_sym x s.asmp.symbols in
      let new_mem = Mem.write x new_sym s.time s.m in
      let new_assumption_set = {symbols=new_symbols; assumptions=s.asmp.assumptions} in
      [{c=Skip; m=new_mem; asmp=new_assumption_set}], Eps)

  | Assert (e,original) ->
      (let handle_failure asmp =
        Printf.eprintf "\027[91mASSERT FAILED\027[0m\n";
        Sym_error.report (Sym_error.Assert original) s.m asmp in 
      match e with
        Val (Conc x) -> if x == 0 then
            (handle_failure s.asmp; [], Deadend)
          else ([{c=Skip; m=s.m; asmp=s.asmp}], Eps)
      | Val symv -> 
          let asmp_false = add_if_assumption symv false s.asmp.symbols s.asmp.assumptions in
          if (check asmp_false) then
            handle_failure {symbols=s.asmp.symbols; assumptions=asmp_false};
          let asmp_true = add_if_assumption symv true s.asmp.symbols s.asmp.assumptions in
          if (check asmp_true) then
            let asmp' = {symbols=s.asmp.symbols; assumptions=asmp_true} in
            [{c=Skip; m=s.m; asmp=asmp'}], Eps
          else [], Deadend
      | _ ->
          let oconfigs = step_exp {e; time=s.time; m=s.m; asmp=s.asmp} in
          (Exp_output_config_set.fold
            (fun {e=e'; m=m'; asmp=asmp'} s ->
              {c=Assert(e',original);m=m';asmp=asmp'}::s)
            oconfigs []), Eps)

(******************************************************************************)

let from_singleton_list = function [x] -> x | _ -> assert false

let step_thread_pool (s:thread_pool_config) : thread_pool_config list =
  let r, tp' = Thread_pool.choose s.tp in
  match r with
  | None -> []
  | Some (id,(Skip,_)) -> assert false
  | Some (id,(c,time)) ->
      let iconfig = {c; time; m=s.m; asmp=s.asmp} in
      let oconfigs,anno = step_thread iconfig in
      match anno with
      | Fork (id',c') ->
          let oconfig = from_singleton_list oconfigs in 
          assert (oconfig.asmp = s.asmp);
          let time_for_id = Clock.inc id time in
          let time_for_id' = Clock.inc id' time in
          let tp'' = Thread_pool.update id' (c',time_for_id')
            (Thread_pool.update id (oconfig.c,time_for_id) tp') in
          [{tp=tp''; m=oconfig.m; ls=s.ls; asmp=oconfig.asmp}]

      | Join id' ->
          let oconfig = from_singleton_list oconfigs in 
          assert (oconfig.asmp = s.asmp);
          assert (oconfig.m = iconfig.m);
          let (c',time') = Thread_pool.lookup id' tp' in
          let r =
            match c' with
            | Skip ->
                let tp'' = Thread_pool.update id (oconfig.c,Clock.join time time') tp' in
                {tp=tp''; m=oconfig.m; ls=s.ls; asmp=oconfig.asmp}
            | _ ->
                (* can't progress *)
                let tp'' = Thread_pool.update id (c, time) tp' in
                {tp=tp''; m=s.m; ls=s.ls; asmp=s.asmp} in
          [r]
          
      | Lock x ->
          let oconfig = from_singleton_list oconfigs in 
          assert (oconfig.asmp = s.asmp);
          assert (oconfig.m = iconfig.m);
          let owner,cnt,time' = Lock_state.lookup x s.ls in
          let id' = match owner with None -> id | Some id' -> id' in
          let r = if id = id' then
            let tp'' = Thread_pool.update id (oconfig.c,Clock.join time time') tp' in
            let ls' = Lock_state.update x (Some id,cnt + 1,time') s.ls in
            {tp=tp''; m=oconfig.m; ls=ls'; asmp=oconfig.asmp}
          else
            (* can't progress *)
            let tp'' = Thread_pool.update id (c, time) tp' in
            {tp=tp''; m=s.m; ls=s.ls; asmp=s.asmp} in
          [r]

      | Unlock x ->
          let oconfig = from_singleton_list oconfigs in 
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
          let tp'' = Thread_pool.update id (oconfig.c,Clock.inc id time) tp' in
          let r = {tp=tp''; m=oconfig.m; ls=ls'; asmp=oconfig.asmp} in
          [r]

      | Eps ->
          (* TODO we are currently not incrementing the time here. This matches
           * the adversarial memory paper semantics but not ours *)
          List.fold_right
            (fun {c=c'; m=m'; asmp=asmp'} a ->
              let tp'' = Thread_pool.update id (c',time) tp' in 
              {tp=tp''; m=m'; ls=s.ls; asmp=asmp'}::a)
            oconfigs []

      | Deadend -> []

(******************************************************************************)

let step_sym_exec (s:thread_pool_config Queue.t) : unit =
  let e = Queue.take s in
  let new_states = step_thread_pool e in
  List.iter (fun s' -> Queue.add s' s) new_states 

(******************************************************************************)

let run (p:program) =
  let id = Thread_pool.new_id () in
  let tp = Thread_pool.update id (p,Clock.inc id Clock.bot) Thread_pool.initial in
  let config = {tp=tp; m=Mem.empty; ls=Lock_state.initial;
                asmp={symbols=TermMap.empty; assumptions=[]}} in
  let s = Queue.create () in
  Queue.add config s; 
  let rec loop () =
    if not (Queue.is_empty s)
    then loop (step_sym_exec s) in
  loop ()

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
  (* make sure that if we're interrupted we still dump error report *)
  Sys.catch_break true;
  let prog = parse_file () in
  (try run prog with Sys.Break -> ());
  Sym_error.dump stdout 
