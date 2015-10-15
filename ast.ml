type var = string

type binop =
  Add
| Sub
| Mul
| Div
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte
| And
| Or

type exp =
  Var of var
| Sym of var
| Conc of int
| Binop of exp * binop * exp

type cmd =
  Skip
| Assign of var * exp
| Seq of cmd * cmd
| If of exp * cmd * cmd
| While of exp * cmd
| Fork of var * cmd
| Join of exp
| Lock of var
| Unlock of var
| Return of exp

type program = cmd
