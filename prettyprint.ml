open Ast

let rec pp_exp ((e,_):exp) : string =
  let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
    | And -> "&&"
    | Or -> "||" in
  let pp_value = function
    | Sym x -> x
    | Conc i -> string_of_int i in
  match e with
  | Var x -> x
  | Val v -> pp_value v
  | Binop (e1,b,e2) -> "("^(pp_exp e1)^(pp_binop b)^(pp_exp e2)^")"

let rec pp_cmd ((c,_):cmd) : string =
  match c with
  | Skip -> "skip"
  | Assign (v,e) -> v^"="^(pp_exp e)
  | Seq (c1,c2) -> (pp_cmd c1)^"; "^(pp_cmd c2)
  | If (e,c1,c2) -> "if "^(pp_exp e)^" then "^(pp_cmd c1)^" else "^(pp_cmd c2)
  | While (e,c) -> "while "^(pp_exp e)^" do "^(pp_cmd c)^" done"
  | Fork (v,c) -> "fork "^v^" do "^(pp_cmd c)^" done"
  | Join e -> "join "^(pp_exp e)
  | Lock v -> "lock "^v
  | Unlock v -> "unlock "^v
  | Symbolic v -> "symbolic "^v
  | Assert e -> "assert "^(pp_exp e)
