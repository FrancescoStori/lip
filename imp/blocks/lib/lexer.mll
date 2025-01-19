{
open Parser
}

let white = [' ' '\n' '\t']+

let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num= ['0'-'9']|['1'-'9']['0'-'9']*

rule read =
  parse
  | white { read lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{"  { LBLOCK }
  | "}"  { RBLOCK }
  | "int" { INT }
  | "bool" { BOOL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "=" { EQ }
  | "<=" { LEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "skip" { SKIP }
  | ":=" { TAKES }
  | ";" { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
