/* Parser for ??? */

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
%token TRUE FALSE
%token SKIP
%token IF THEN ELSE FI
%token WHILE DO
%token FORK JOIN
%token LOCK UNLOCK
%token PLUS MINUS
%token TIMES DIVIDE
%token EQ NEQ LT LTE GT GTE
%token AND OR
%token ASSIGN
%token LBRACE RBRACE LPAREN RPAREN 
%token SEMICOLON
%token RETURN
%token DONE

%right IF THEN ELSE FI
%right WHILE DO DONE
%right FORK JOIN
%right LOCK UNLOCK
%left PLUS MINUS
%left TIMES DIVIDE
%right EQ NEQ LT LTE GT GTE
%right AND OR
%right ASSIGN
%right LBRACE RBRACE LPAREN RPAREN 
%left SEMICOLON

%%

program:
  cmd EOF  { $1 }

exp:
    ID                { Ast.Var($1) }
  | INT               { Ast.Val(Conc($1)) }
  | exp PLUS exp      { Ast.Binop($1, Ast.Add, $3) }
  | exp MINUS exp     { Ast.Binop($1, Ast.Sub, $3) }
  | exp TIMES exp     { Ast.Binop($1, Ast.Mul, $3) }
  | exp DIVIDE exp    { Ast.Binop($1, Ast.Div, $3) }
  | exp EQ exp        { Ast.Binop($1, Ast.Eq, $3) }
  | exp NEQ exp       { Ast.Binop($1, Ast.Neq, $3) }
  | exp LT exp        { Ast.Binop($1, Ast.Lt, $3) }
  | exp LTE exp       { Ast.Binop($1, Ast.Lte, $3) }
  | exp GT exp        { Ast.Binop($1, Ast.Gt, $3) }
  | exp GTE exp       { Ast.Binop($1, Ast.Gte, $3) }
  | exp AND exp       { Ast.Binop($1, Ast.And, $3) }
  | exp OR exp        { Ast.Binop($1, Ast.Or, $3) }
  | LPAREN exp RPAREN { ($2) }

cmd:
    SKIP                          { Ast.Skip }
  | ID ASSIGN exp                 { Ast.Assign($1, $3) }
  | IF exp THEN cmd ELSE cmd FI   { Ast.If($2, $4, $6) }
  | WHILE exp DO cmd DONE         { Ast.While($2, $4) }
  | FORK ID DO cmd DONE           { Ast.Fork($2, $4) }
  | JOIN exp                      { Ast.Join($2) }
  | LOCK ID                       { Ast.Lock($2) }
  | UNLOCK ID                     { Ast.Unlock($2) }
  | LBRACE cmd RBRACE             { ($2) }
  | cmd SEMICOLON                 { Ast.Seq($1, Ast.Skip) }
  | cmd SEMICOLON cmd             { Ast.Seq($1, $3) }
  | RETURN exp                    { Ast.Return($2) }
  | SYMBOLIC ID                   { Ast.Symbolic($2) }
  | ASSERT exp                    { Ast.Assert($2) }
