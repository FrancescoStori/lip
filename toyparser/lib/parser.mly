%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token TIMES
%token DIVIDED
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left MINUS
%left TIMES
%left DIVIDED

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | MINUS; e = expr { Neg(e) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIVIDED; e2 = expr { Div(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
