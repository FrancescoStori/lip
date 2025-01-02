%{
    open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token LPAREN
%token RPAREN
%token VAR 
%token CONST
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
    | TRUE { True }
    | FALSE { False }
    | VAR; e = expr { Var e }
    | CONST; e = expr { Const e }
    | NOT; e = expr { Not e }
    | e1 = expr; AND; e2 = expr { And(e1, e2) }
    | e1 = expr; OR; e2 = expr { Or(e1, e2) }
    | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
    | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
    | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
    | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
    | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
    | LPAREN; e=expr; RPAREN {e}
;