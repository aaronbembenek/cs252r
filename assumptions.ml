(* http://cubicle.lri.fr/alt-ergo-zero/ *)

open Aez
open Smt
open Ast
module T = Term
module F = Formula
module Solver = Make (struct end)

module TermMap = Map.Make(String);;
type termMap = Smt.Term.t TermMap.t

(* Given the name of a variable and map of accumulated symbolic values, return
   the corresponding term and updated map. *)
let make_sym (x : string) (symbols : termMap) : Smt.Term.t * termMap =
	if TermMap.mem x symbols
	then (TermMap.find x symbols, symbols)
	else let sym = Hstring.make x in
		Symbol.declare sym [] Type.type_int;
		let term = T.make_app sym [] in
		(term, TermMap.add x term symbols)
;;

(* z = x binop y *)
let make_sym_binop (x : string) (y : string) (z : string) (symbols : termMap)
		(b : binop) =
	let (tx, symbols_x) = make_sym x symbols in
	let (ty, symbols_y) = make_sym y symbols_x in
	let (tz, symbols_z) = make_sym y symbols_y in
	match b with
		Add -> F.make_lit F.Eq [tz; T.make_arith T.Plus tx ty]
	| Sub -> F.make_lit F.Eq [tz; T.make_arith T.Minus tx ty]
	| Mul -> F.make_lit F.Eq [tz; T.make_arith T.Mult tx ty]
	| Div -> F.make_lit F.Eq [tz; T.make_arith T.Div tx ty]

(* x bincmp y *)
let make_sym_binop (x : string) (y : string) (symbols : termMap) (b : bincmp) =
	let (tx, symbols_x) = make_sym x symbols in
	let (ty, symbols_y) = make_sym y symbols_x in
	match b with
		Eq -> F.make_lit F.Eq [tx; ty]
	| Neq -> F.make_lit F.Neq [tx; ty]
	| Lt -> F.make_lit F.Lt [tx; ty]
	| Lte -> F.make_lit F.Le [tx; ty]
	| Gt -> F.make F.Not [F.make_lit F.Le [tx; ty]]
	| Gte -> F.make F.Not [F.make_lit F.Lt [tx; ty]]
	| And -> F.make F.And [F.make_lit F.Neq [tx; T.make_int (Num.Int 0)];
			F.make_lit F.Neq [ty; T.make_int (Num.Int 0)]]
	| Or -> F.make F.Or [F.make_lit F.Neq [tx; T.make_int (Num.Int 0)];
			F.make_lit F.Neq [ty; T.make_int (Num.Int 0)]]
;;

let x = Hstring.make "x";;
let y = Hstring.make "y";;

Symbol.declare x [] Type.type_int;;
Symbol.declare y [] Type.type_int;;

let tx = T.make_app x [];;
let ty = T.make_app y [];;
let t1 = T.make_int (Num.Int 1);;

let f = F.make_lit F.Eq [tx; (T.make_arith T.Minus ty t1)];;  (* x = y - 1 *)
let neg_goal = F.make F.Not [F.make_lit F.Lt [tx; ty]];;      (* not (x < y) *)

try
  Solver.clear ();
  Solver.assume ~id:1 f;
  Solver.assume ~id:2 neg_goal;
  Solver.check ();
  print_endline "not valid"
with Unsat _ ->
  print_endline "valid";;