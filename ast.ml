type var = string
type pos = int

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

type value =
  Sym of var
| Conc of int

type rexp =
  Var of var
| Val of value
| Binop of exp * binop * exp
and exp = rexp*pos

type rcmd =
  Skip
| Assign of var * exp
| Seq of cmd * cmd
| If of exp * cmd * cmd
| While of exp * cmd
| Fork of var * cmd
| Join of exp
| Lock of var
| Unlock of var
| Symbolic of var
| Assert of exp
and cmd = rcmd*pos

type program = cmd
