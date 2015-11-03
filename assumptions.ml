(* http://cubicle.lri.fr/alt-ergo-zero/ *)
open Aez
open Smt
open Ast
open Format
module T = Term
module F = Formula
module Solver = Make (struct end)

exception Runtime_exception of string

module TermMap = Map.Make(String) ;;
type termMap = Smt.Term.t TermMap.t ;;

type assumptions = Smt.Formula.t list ;;

let one = T.make_int (Num.Int 1) ;;
let zero = T.make_int (Num.Int 0) ;;

let counter = ref 0 ;;

let make_sym (x : string) (symbols : termMap) : value * termMap =
  let new_symbols = (if TermMap.mem x symbols
    then symbols
    else let sym = Hstring.make x in
      Symbol.declare sym [] Type.type_int;
      let term = T.make_app sym [] in
      TermMap.add x term symbols) in
  (Sym x, new_symbols)
;;

let get_new_sym (symbols : termMap) : value * termMap =
  let new_sym = ("_s" ^ (string_of_int !counter)) in
  counter := (!counter) + 1;
  make_sym new_sym symbols
;;

let get_term (x : value) (symbols : termMap) : Smt.Term.t =
  match x with
    Sym v -> (if TermMap.mem v symbols
      then TermMap.find v symbols
      else raise (Runtime_exception "uninitialized symbolic value"))
  | Conc i -> T.make_int (Num.Int i)
;;

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

let add_if_assumption (x : value) (b : bool) (symbols : termMap)
    (assumps : assumptions) : assumptions =
  let tx = get_term x symbols in
  let assump = 
    (if b then F.make_lit F.Neq [tx; zero] else F.make_lit F.Eq [tx; zero]) in
  assumps @ [assump]
;;

(* Debugging! *)
let rec print_formulas (assumptions : Smt.Formula.t list) : unit =
  match assumptions with
    [] -> ()
  | hd::tl -> Smt.Formula.print Format.std_formatter hd; 
      print_string "\n";
      print_formulas tl
;;

let rec make_assumptions (assumptions : Smt.Formula.t list) (n : int) =
  match assumptions with
    [] -> ()
  | hd::tl -> Solver.assume ~id:n hd; make_assumptions tl (n + 1)
;;

let check (assumptions : Smt.Formula.t list) : bool =
  try
    Solver.clear ();
    make_assumptions assumptions 1;
    Solver.check ();
    true
  with Unsat _ ->
    false
;;

let is_div_by_zero (v1:value) (b:binop) (v2:value) (syms:termMap) (assumps:assumptions) : bool =
	match b, v2 with
		Div, Conc x -> x == 0
	| Div, Sym x -> check (add_if_assumption (Sym x) false syms assumps)
	| _ -> false
