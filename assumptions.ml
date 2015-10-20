(* http://cubicle.lri.fr/alt-ergo-zero/ *)
open Aez
open Smt
open Ast
module T = Term
module F = Formula
module Solver = Make (struct end)

module TermMap = Map.Make(String) ;;
type termMap = Smt.Term.t TermMap.t ;;

type assumptions = Smt.Formula.t list ;;

let one = T.make_int (Num.Int 1) ;;
let zero = T.make_int (Num.Int 0) ;;

let rec get_new_sym (symbols : termMap) (i : int) : value =
  let new_sym = ("_s" ^ (string_of_int i)) in
  if TermMap.mem new_sym symbols
  then get_new_sym symbols (i + 1)
  else Sym(new_sym)

let make_sym (x : value) (symbols : termMap) : Smt.Term.t * termMap =
  match x with
    Sym v ->
      (if TermMap.mem v symbols
      then (TermMap.find v symbols, symbols)
      else let sym = Hstring.make v in
        Symbol.declare sym [] Type.type_int;
        let term = T.make_app sym [] in
        (term, TermMap.add v term symbols))
  | Conc i -> (T.make_int (Num.Int i), symbols)
;;

let add_binop_assumption (x : value) (y : value) (symbols : termMap)
    (assumps : assumptions) (b : binop) : value * termMap * assumptions =
  let (tx, symbols_x) = make_sym x symbols in
  let (ty, symbols_y) = make_sym y symbols_x in
  let new_sym = get_new_sym symbols_y 0 in
  let (tz, symbols_z) = make_sym new_sym symbols_y in
  let assump = 
    (match b with
      Add -> F.make_lit F.Eq [tz; T.make_arith T.Plus tx ty]
    | Sub -> F.make_lit F.Eq [tz; T.make_arith T.Minus tx ty]
    | Mul -> F.make_lit F.Eq [tz; T.make_arith T.Mult tx ty]
    | Div -> F.make_lit F.Eq [tz; T.make_arith T.Div tx ty]
    | _ -> let f = (match b with
      | Eq -> F.make_lit F.Eq [tx; ty]
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
  (new_sym, symbols_z, assumps @ [assump])
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

(*match (check assumptions) with
  true -> print_endline "valid"
| false -> print_endline "not valid"*)