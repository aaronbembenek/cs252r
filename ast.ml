type var = string

type binop =
  Add
| Sub
| Mul
| Div

type cmp =
  Eq
| Neq
| Lt
| Lte
| Gt
| Gte

type aexp =
  Int of int
| Var of var
| Binop of aexp * binop * aexp

type bexp =
  True
| False
| Cmp of aexp * cmp * aexp
| And of bexp * bexp
| Or of bexp * bexp

type cmd =
  Skip
| Assign of var * aexp
| Seq of cmd * cmd
| If of bexp * cmd * cmd
| While of bexp * cmd
| Fork of cmd
| Join of aexp