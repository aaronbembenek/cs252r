open Ast

exception Done of int

(* Map for variables with concrete bindings. *)
let cvars = Hashtbl.create 33
let lookup_cvars x =
  try (Hashtbl.find cvars x)
  with Not_found ->
    let r = ref 0 in
    Hashtbl.add cvars x r;
    r
let set_cvars x i = let r = lookup_cvars x in r := i

(* Data structures for keeping track of threads. *)
let cur_tid = ref 0
let create_new_tid () =
  let tid = !cur_tid in cur_tid := !cur_tid + 1; tid
let threads : (int, (cmd list) ref) Hashtbl.t = Hashtbl.create 33
let get_thread tid = Hashtbl.find threads tid
let add_thread tid c = Hashtbl.add threads tid (ref [c])
let rm_thread tid = Hashtbl.remove threads tid
let is_thread_alive tid =
  try (let _ = Hashtbl.find threads tid in true)
  with Not_found -> false
let get_active_tids () = Hashtbl.fold (fun tid _ l -> tid :: l) threads []

let rec eval_aexp (e : aexp) : int =
  match e with
    Int i -> i
  | Var x -> !(lookup_cvars x)
  | Binop(e1,b,e2) ->
      let (i1,i2) = (eval_aexp e1, eval_aexp e2) in
        match b with
          Add -> i1 + i2
        | Sub -> i1 - i2
        | Mul -> i1 * i2
        | Div -> i1 / i2

let rec eval_bexp (e : bexp) : bool =
  match e with
    True -> true
  | False -> false
  | And(e1,e2) -> (eval_bexp e1) && (eval_bexp e2)
  | Or(e1,e2) -> (eval_bexp e1) || (eval_bexp e2)
  | Cmp(e1,cmp,e2) ->
      let (i1,i2) = (eval_aexp e1, eval_aexp e2) in
      match cmp with
        Eq -> i1 == i2
      | Neq -> i1 != i2
      | Lt -> i1 < i2
      | Lte -> i1 <= i2
      | Gt -> i1 > i2
      | Gte -> i1 >= i2

let step_thread (tid : int) : unit =
  let cmds_ref = get_thread tid in
  match !cmds_ref with
  | [] -> rm_thread tid
  | c :: cmds ->
    match c with
    | Skip -> cmds_ref := cmds
    | Assign(x,e) -> let i = eval_aexp e in set_cvars x i; cmds_ref := cmds
    | Seq(c1,c2) -> cmds_ref := c1 :: c2 :: cmds
    | If(b,c1,c2) -> let nxt_cmd = if (eval_bexp b) then c1 else c2 in cmds_ref := nxt_cmd :: cmds
    | While(b,c') -> cmds_ref := If(b,Seq(c',c),Skip) :: cmds
    | Fork (x,c') ->
        let tid' = create_new_tid () in add_thread tid' c'; set_cvars x tid'; cmds_ref := cmds
    | Join n -> if not (is_thread_alive (eval_aexp n)) then cmds_ref := cmds
    | Return n -> raise (Done (eval_aexp n))

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
