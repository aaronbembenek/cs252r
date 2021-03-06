(* Information on Aez can be found at http://cubicle.lri.fr/alt-ergo-zero/. *)
open Aez
open Smt
open Ast
open Format
open State
module T = Term
module F = Formula
module Solver = Make (struct end)

exception Runtime_exception of string

let one = T.make_int (Num.Int 1) ;;
let zero = T.make_int (Num.Int 0) ;;

(* Count of symbolic variables. Used to generate unique symbolic variable
   names. *)
let counter = ref 0 ;;

(* Create new symbolic variable with a given name. *)
let make_sym (x : string) (symbols : State.termMap) : value * termMap =
  let new_symbols = (if TermMap.mem x symbols
    then symbols
    else let sym = Hstring.make x in
      Symbol.declare sym [] Type.type_int;
      let term = T.make_app sym [] in
      TermMap.add x term symbols) in
  (Sym x, new_symbols)
;;

(* Create new symbolic variable with the next available name. *)
let get_new_sym (symbols : termMap) : value * termMap =
  let new_sym = ("_s" ^ (string_of_int !counter)) in
  counter := (!counter) + 1;
  make_sym new_sym symbols
;;

(* Get SMT term associated with the given value. *)
let get_term (x : value) (symbols : termMap) : Smt.Term.t =
  match x with
    Sym v -> (if TermMap.mem v symbols
      then TermMap.find v symbols
      else raise (Runtime_exception "uninitialized symbolic value"))
  | Conc i -> T.make_int (Num.Int i)
;;

(* Add a constraint to given assumps: (var = v1 or var = v2 or ... or var = vn)
   where v1, v2, ..., vn are the elements of val. *)
let add_read_disjunction (var : var) (vals : State.Value_set.t)
    (symbols : termMap) (assumps : assumptions)
    : value * termMap * assumptions = 
  let rec disjunction_helper (tz : Smt.Term.t) (symbols : termMap) (v : value)
      (acc : assumptions) : assumptions =
    acc @ [F.make_lit F.Eq [tz; get_term v symbols]] in
  let (new_sym, new_symbols) = get_new_sym symbols in
  let tz = get_term new_sym new_symbols in
  let disjunction = 
    State.Value_set.fold (disjunction_helper tz symbols) vals [F.f_false] in
  (new_sym, new_symbols, assumps @ [F.make F.Or disjunction])
;;

(* Add a binop constraint to assumps. *)
let add_binop_assumption (x : value) (y : value) (symbols : termMap)
    (assumps : assumptions) (b : binop) : value * termMap * assumptions =
  let tx = get_term x symbols in
  let ty = get_term y symbols in
  let (new_sym, new_symbols) = get_new_sym symbols in
  let tz = get_term new_sym new_symbols in
  let assump = 
    (match b with
      Add -> F.make_lit F.Eq [tz; T.make_arith T.Plus tx ty]
    | Sub -> F.make_lit F.Eq [tz; T.make_arith T.Minus tx ty]
    | Mul -> F.make_lit F.Eq [tz; T.make_arith T.Mult tx ty]
    | Div -> F.make_lit F.Eq [tz; T.make_arith T.Div tx ty]
    | _ -> let f = (match b with
        Eq -> F.make_lit F.Eq [tx; ty]
      | Neq -> F.make_lit F.Neq [tx; ty]
      | Lt -> F.make_lit F.Lt [tx; ty]
      | Lte -> F.make_lit F.Le [tx; ty]
      | Gt -> F.make F.Not [F.make_lit F.Le [tx; ty]]
      | Gte -> F.make F.Not [F.make_lit F.Lt [tx; ty]]
      | And -> F.make F.And [F.make_lit F.Neq [tx; zero];
          F.make_lit F.Neq [ty; zero]]
      | Or -> F.make F.Or [F.make_lit F.Neq [tx; zero];
          F.make_lit F.Neq [ty; zero]]
      | _ -> assert false) in
      F.make_lit F.Eq [tz; T.make_ite f one zero]
    ) in
  (new_sym, new_symbols, assumps @ [assump])
;;

(* Add an if constraint to assumps. *)
let add_if_assumption (x : value) (b : bool) (symbols : termMap)
    (assumps : assumptions) : assumptions =
  let tx = get_term x symbols in
  let assump = 
    (if b then F.make_lit F.Neq [tx; zero] else F.make_lit F.Eq [tx; zero]) in
  assumps @ [assump]
;;

(* Debugging function that prints SMT formulas. *)
let rec print_formulas (assumptions : Smt.Formula.t list) : unit =
  match assumptions with
    [] -> ()
  | hd::tl -> Smt.Formula.print Format.std_formatter hd; 
      print_string "\n";
      print_formulas tl
;;

(* Return true if the given formula is satisfiable, false otherwise. *)
let check (assumptions : Smt.Formula.t list) : bool =
  let rec make_assumptions (assumptions : Smt.Formula.t list) (n : int) =
  match assumptions with
    [] -> ()
  | hd::tl -> Solver.assume ~id:n hd; make_assumptions tl (n + 1) in
  try
    Solver.clear ();
    make_assumptions assumptions 1;
    Solver.check ();
    true
  with Unsat _ ->
    false
;;