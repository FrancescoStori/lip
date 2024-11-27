{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "&&" { AND }
  | "||" { OR }
  | "iszero" {ISZERO}
  | "0" {ZERO}
  | "pred" {PRED}
  | "succ" {SUCC}
  | eof { EOF }