%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
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

%left OR
%left AND
%nonassoc NOT
%left ISZERO
%left SUCC PRED

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
  | SUCC ; e = expr; { Succ(e) }
  | PRED ; e = expr; { Pred(e) }
  | ISZERO; e = expr; {IsZero(e)}
  | NOT; e = expr; { Not(e) }
  | LPAREN; e=expr; RPAREN {e}
;
