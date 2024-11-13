{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = "0x"['0'-'9' 'a'-'f']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDED }
  | hex { CONST (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
