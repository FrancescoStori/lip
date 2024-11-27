%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token SUCC
%token PRED
%token ISZERO
%token ZERO
%token EOF

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e1 = expr; AND; e2 = expr; { If(e1, e2, False) }
  | e1 = expr; OR; e2 = expr; { If(e1, True, e2) }
  | ZERO { Zero }
  | ISZERO; e = expr; {IsZero(e)}
  | SUCC ; e = expr; { Succ(e) }
  | PRED ; e = expr; { Pred(e) }
  | LPAREN; e=expr; RPAREN {e}
;

