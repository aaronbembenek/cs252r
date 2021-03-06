%{
open Ast
open Lexing
(* Returns the line number for the n'th token. *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Specify which non-terminal to start the grammar with. */
%start program

/* Specify the non-terminals of the grammar and the types of the values they build. */
%type <Ast.program> program
%type <Ast.exp> exp
%type <Ast.cmd> cmd

/* Give a definition of all of the terminals (i.e., tokens) in the grammar. */
%token <int> INT 
%token EOF
%token <string> ID
%token SYMBOLIC
%token ASSERT
%token SKIP
%token IF THEN ELSE FI
%token WHILE DO
%token FORK JOIN
%token LOCK UNLOCK
%token PLUS MINUS
%token TIMES DIVIDE
%token AND OR
%token EQ NEQ LT LTE GT GTE
%token ASSIGN
%token LBRACE RBRACE LPAREN RPAREN 
%token SEMICOLON
%token DONE
%token NOT

%right AND OR
%right EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%right SEMICOLON

%%

program:
  cmd EOF  { $1 }

primary:
    ID                { Ast.Var($1), rhs 1 }
  | INT               { Ast.Val(Conc($1)), rhs 1 }
  | LPAREN exp RPAREN { ($2) }

exp:
    primary           { $1 }
  | MINUS primary     { Ast.Binop($2, Ast.Mul, (Ast.Val(Conc(-1)), rhs 1)), rhs 1 }
  | exp PLUS exp      { Ast.Binop($1, Ast.Add, $3), rhs 1 }
  | exp MINUS exp     { Ast.Binop($1, Ast.Sub, $3), rhs 1 }
  | exp TIMES exp     { Ast.Binop($1, Ast.Mul, $3), rhs 1 }
  | exp DIVIDE exp    { Ast.Binop($1, Ast.Div, $3), rhs 1 }
  | exp AND exp       { Ast.Binop($1, Ast.And, $3), rhs 1 }
  | exp OR exp        { Ast.Binop($1, Ast.Or, $3), rhs 1 }
  | exp EQ exp        { Ast.Binop($1, Ast.Eq, $3), rhs 1 }
  | exp NEQ exp       { Ast.Binop($1, Ast.Neq, $3), rhs 1 }
  | exp LT exp        { Ast.Binop($1, Ast.Lt, $3), rhs 1 }
  | exp LTE exp       { Ast.Binop($1, Ast.Lte, $3), rhs 1 }
  | exp GT exp        { Ast.Binop($1, Ast.Gt, $3), rhs 1 }
  | exp GTE exp       { Ast.Binop($1, Ast.Gte, $3), rhs 1 }
  | NOT exp           { Ast.Binop($2, Ast.Eq, (Ast.Val(Conc(0)), rhs 1)), rhs 1 }

cmd:
    SKIP                          { Ast.Skip, rhs 1 }
  | ID ASSIGN exp                 { Ast.Assign($1, $3), rhs 1 }
  | IF exp THEN cmd ELSE cmd FI   { Ast.If($2, $4, $6), rhs 1 }
  | WHILE exp DO cmd DONE         { Ast.While($2, $4), rhs 1 }
  | FORK ID DO cmd DONE           { Ast.Fork($2, $4), rhs 1 }
  | JOIN exp                      { Ast.Join($2), rhs 1 }
  | LOCK ID                       { Ast.Lock($2), rhs 1 }
  | UNLOCK ID                     { Ast.Unlock($2), rhs 1 }
  | LBRACE cmd RBRACE             { ($2) }
  | cmd SEMICOLON                 { Ast.Seq($1, (Ast.Skip, rhs 1)), rhs 1 }
  | cmd SEMICOLON cmd             { Ast.Seq($1, $3), rhs 1 }
  | SYMBOLIC ID                   { Ast.Symbolic($2), rhs 1 }
  | ASSERT exp                    { Ast.Assert($2), rhs 1 }
