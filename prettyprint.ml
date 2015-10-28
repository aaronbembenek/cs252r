open Ast

let rec pp_exp (e:exp) : string =
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
