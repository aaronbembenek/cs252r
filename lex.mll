(* Lexer for ??? *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* Definitions *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let alpha=['a'-'z' 'A'-'Z']
let identifier=(alpha)(digit|alpha|'_')*

(* Rules *)
rule lexer = parse
| eol               { incr_lineno lexbuf; lexer lexbuf } 
| ws+               { lexer lexbuf }
| digit+            { INT(int_of_string(Lexing.lexeme lexbuf)) }
| "true"            { TRUE }
| "false"           { FALSE }
| "skip"            { SKIP }
| "if"              { IF }
| "then"            { THEN }
| "else"            { ELSE }
| "while"           { WHILE }
| "do"              { DO }
| "fork"            { FORK }
| "join"            { JOIN }
| "return"          { RETURN }
| identifier as id  { ID(id) }
| "+"               { PLUS }
| "-"               { MINUS }
| "*"               { TIMES }
| "/"               { DIVIDE }
| "=="              { EQ }
| "!="              { NEQ }
| "<"               { LT }
| "<="              { LTE }
| ">"               { GT }
| ">="              { GTE }
| "&&"              { AND }
| "||"              { OR }
| "="               { ASSIGN }
| "{"               { LBRACE }
| "}"               { RBRACE }
| "("               { LPAREN }
| ")"               { RPAREN }
| ";"               { SEMICOLON }
| "/*"              { comment lexbuf }
| eof               { EOF }

and comment = parse
| _                 { comment lexbuf }
| "*/"              { lexer lexbuf }
| eof               { raise (Failure "Unmatched /*") }
