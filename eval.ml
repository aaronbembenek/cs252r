open Ast

let cvars = Hashtbl.create 33

let lookup_cvars x =
  try (Hashtbl.find cvars x)
  with Not_found ->
    let r = ref 0 in
    Hashtbl.add cvars x r;
    r

let set_cvars x i = let r = lookup_cvars x in r := i

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

let rec eval_cmd (c : cmd) : unit =
  match c with
  | Skip -> ()
  | Assign(x,e) -> let i = eval_aexp e in set_cvars x i
  | Seq(c1,c2) -> eval_cmd c1; eval_cmd c2
  | If(b,c1,c2) -> if (eval_bexp b) then eval_cmd c1 else eval_cmd c2
  | While(b,c') -> eval_cmd (If(b,Seq(c',c),Skip))
  | Fork c -> ()
  | Join tid -> ()
