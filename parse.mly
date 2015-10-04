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
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp
%type <Ast.cmd> cmd

/* Give a definition of all of the terminals (i.e., tokens) in the grammar. */
%token <int> INT 
%token EOF
%token <string> ID
%token TRUE FALSE
%token SKIP
%token IF THEN ELSE FI
%token WHILE DO
%token FORK JOIN
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

aexp:
    ID                  { Ast.Var($1) }
  | INT                 { Ast.Int($1) }
  | aexp PLUS aexp      { Ast.Binop($1, Ast.Add, $3) }
  | aexp MINUS aexp     { Ast.Binop($1, Ast.Sub, $3) }
  | aexp TIMES aexp     { Ast.Binop($1, Ast.Mul, $3) }
  | aexp DIVIDE aexp    { Ast.Binop($1, Ast.Div, $3) }
  | LPAREN aexp RPAREN  { ($2) }

bexp:
    TRUE            { Ast.True }
  | FALSE           { Ast.False }
  | aexp EQ aexp    { Ast.Cmp($1, Ast.Eq, $3) }
  | aexp NEQ aexp   { Ast.Cmp($1, Ast.Neq, $3) }
  | aexp LT aexp    { Ast.Cmp($1, Ast.Lt, $3) }
  | aexp LTE aexp   { Ast.Cmp($1, Ast.Lte, $3) }
  | aexp GT aexp    { Ast.Cmp($1, Ast.Gt, $3) }
  | aexp GTE aexp   { Ast.Cmp($1, Ast.Gte, $3) }
  | bexp AND bexp   { Ast.And($1, $3) }
  | bexp OR bexp    { Ast.Or($1, $3) }

cmd:
    SKIP                          { Ast.Skip }
  | ID ASSIGN aexp                { Ast.Assign($1, $3) }
  | IF bexp THEN cmd ELSE cmd FI  { Ast.If($2, $4, $6) }
  | WHILE bexp DO cmd DONE        { Ast.While($2, $4) }
  | FORK ID DO cmd DONE           { Ast.Fork($2, $4) }
  | JOIN aexp                     { Ast.Join($2) }
  | cmd SEMICOLON cmd             { Ast.Seq($1, $3) }
  | RETURN aexp                   { Ast.Return($2) }
