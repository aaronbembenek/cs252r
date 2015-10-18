(* http://cubicle.lri.fr/alt-ergo-zero/ *)

open Aez
open Smt
module T = Term
module F = Formula
module Solver = Make (struct end)

let x = Hstring.make "x";;
let y = Hstring.make "y";;

Symbol.declare x [] Type.type_int;;
Symbol.declare y [] Type.type_int;;

let tx = T.make_app x [];;
let ty = T.make_app y [];;
let t1 = T.make_int (Num.Int 1);;

let f = F.make_lit F.Eq [tx; (T.make_arith T.Minus ty t1)];; (* x = y - 1 *)
let neg_goal = F.make F.Not [F.make_lit F.Lt [tx; ty]];;      (* not (x < y) *)

try
  Solver.clear ();
  Solver.assume ~id:1 f;
  Solver.assume ~id:2 neg_goal;
  Solver.check ();
  print_endline "not valid"
with Unsat _ ->
  print_endline "valid";;