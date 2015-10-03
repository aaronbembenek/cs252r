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
%token IF THEN ELSE
%token WHILE DO
%token FORK JOIN
%token PLUS MINUS
%token TIMES DIVIDE
%token EQ NEQ LT LTE GT GTE
%token AND OR
%token ASSIGN
%token LBRACE RBRACE LPAREN RPAREN 
%token SEMICOLON

%right IF THEN ELSE
%right WHILE DO
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
  cmd EOF	{ $1 }

aexp:
	  ID                  { (Ast.Var($1), (rhs 1)) }
	| INT                 { (Ast.Int($1), (rhs 1)) }
	| aexp PLUS aexp      { (Ast.Binop($1, Ast.Add, $3), (rhs 2)) }
	| aexp MINUS aexp     { (Ast.Binop($1, Ast.Sub, $3), (rhs 2)) }
	| aexp TIMES aexp     { (Ast.Binop($1, Ast.Mul, $3), (rhs 2)) }
	| aexp DIVIDE aexp    { (Ast.Binop($1, Ast.Div, $3), (rhs 2)) }
	| LPAREN aexp RPAREN  { ($2) }

bexp:
		TRUE            { (Ast.True, (rhs 1)) }
	| FALSE           { (Ast.False, (rhs 1)) }
	| bexp EQ bexp    { (Ast.Cmp($1, Ast.Eq, $3), (rhs 2)) }
	| bexp NEQ bexp   { (Ast.Cmp($1, Ast.Neq, $3), (rhs 2)) }
	| bexp LT bexp    { (Ast.Cmp($1, Ast.Lt, $3), (rhs 2)) }
	| bexp LTE bexp   { (Ast.Cmp($1, Ast.Lte, $3), (rhs 2)) }
	| bexp GT bexp    { (Ast.Cmp($1, Ast.Gt, $3), (rhs 2)) }
	| bexp GTE bexp   { (Ast.Cmp($1, Ast.Gte, $3), (rhs 2)) }
	| bexp AND bexp   { (Ast.And($1, $3), (rhs 2)) }
	| bexp OR bexp    { (Ast.Or($1, $3), (rhs 2)) }

cmd:
		SKIP                        { (Ast.Skip, (rhs 1)) }
	| aexp ASSIGN aexp            { (Ast.Assign($1, $3), (rhs 2)) }
	| IF bexp THEN cmd ELSE cmd   { (Ast.If($2, $4, $6), (rhs 1)) }
	| WHILE bexp DO cmd           { (Ast.While($2, $4), (rhs 1)) }
	| FORK cmd                    { (Ast.Fork($2), (rhs 1)) }
	| JOIN aexp                   { (Ast.Join($2), (rhs 1)) }
	| cmd SEMICOLON cmd           { (Ast.Seq($1, $3), (rhs 2)) }