type var = string

type binop =
  Add
| Sub
| Mul
| Div

type bincmp =
  Eq
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

type exp =
  Var of var
| Val of value
| Binop of exp * binop * exp
| Bincmp of exp * bincmp * exp

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
| Symbolic of var
| Assert of exp

type program = cmd
