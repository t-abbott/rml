%{
  open Typing.Op
  open Ast.Parsetree
  open Typing

  module Ty = Ty_template
%}

%token TNUM
%token TBOOL
%token COLON
%token ARROW
%token <Utils.Ident.t> VAR

%token <float> NUM
%token TRUE FALSE

%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token EQUAL LESS GREATER
%token AND OR

%token IF THEN ELSE
%token FUN
%token LPAREN RPAREN
%token LET VAL IN

%token LINE
%token LBRACKET RBRACKET

%token SEMISEMI
%token EOF

%start file
%type <Ast.Parsetree.program> file


%right ARROW
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES

%%

file:
  | EOF
    { [] }
  | e = topexpr EOF
    { [e] }
  | e = topexpr SEMISEMI lst = file
    { e :: lst }
  | ds = nonempty_list(topdef) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(topdef) EOF
    { ds }

topdef: mark_location(topdef_unmarked) { $1 }
topdef_unmarked:   
  | LET x = VAR EQUAL e = expr
    { LetDef (x, e) }
  | VAL x = VAR COLON t = ty_val
    { ValDef (x, t)}

topexpr: mark_location(topexpr_unmarked) { $1 }
topexpr_unmarked:
| e = expr
    { Expr e }

expr: mark_location(expr_unmarked) { $1 }
expr_unmarked:
  | e = app_expr_unmarked
    { e }
  | MINUS n = NUM
    { Number (-. n) }
  | e1 = expr op = expr_binop e2 = expr
    { Binop (op, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN arg = VAR ARROW body = expr 
    { Fun ([arg], body) }
  | LET name = VAR EQUAL e1 = expr IN e2 = expr
    { LetIn (name, e1, e2) }
  | VAL name = VAR COLON t = ty_val IN e2 = expr 
    { ValIn (name, t, e2) }

%inline
expr_binop:
    | PLUS
        { Binop.Plus }
    | MINUS
        { Binop.Minus }
    | TIMES
        { Binop.Times }
    | DIV  
        { Binop.Div }
    | EQUAL
        { Binop.Equal }
    | LESS 
        { Binop.Less }
    | GREATER
        { Binop.Greater } 
    | AND
        { Binop.And }
    | OR
        { Binop.Or }
    | MOD
        { Binop.Mod }

app_expr: mark_location(app_expr_unmarked) { $1 }
app_expr_unmarked:
  | e = simple_expr_unmarked
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, [e2]) }

simple_expr: mark_location(simple_expr_unmarked) { $1 }
simple_expr_unmarked:
  | x = VAR
    { Var x }
  | TRUE    
    { Boolean true }
  | FALSE
    { Boolean false }
  | n = NUM
    { (Number n) }
  | e = expr COLON t = ty
    { Annotated (e, t) }
  | LPAREN e = expr_unmarked RPAREN	
    { e }    


(*
  ty ::= <refinement>                   // RBase
       | <var>:<refinement> -> <ty>     // RArrow
 *)
ty: mark_annot_location(ty_unmarked) { $1 }
ty_val: mark_val_location(ty_unmarked) { $1 }
ty_unmarked:
  | r = refinement 
    { Ty_template.RBase r }
  | x = VAR COLON s = ty ARROW t = ty
    { Ty_template.RArrow (x, s, t) }
  | LPAREN t = ty_unmarked RPAREN
    { t }

ty_basic:
  | TBOOL
    { Base_ty.TBool }
  | TNUM
    { Base_ty.TInt } 

(*
  refinement ::= <base ty>                      // unrefined (i.e. p=true)
               | <var>:[<var> | <predicate>]    // refined base type
 *)
refinement:
  | t = ty_basic
    { Ty.R.from "v" t None }
  | t = ty_basic LBRACKET v = VAR LINE p = predicate RBRACKET 
    { Ty.R.from v t (Some p) }

(*
  predicate ::= <var> | <bool> | <int>
              | <p> && <p>
              | <p> || <p>
              | <p> <intop> <p>
              | if <p> then <p> else <p>
 *)
predicate: mark_location(predicate_unmarked) { $1 }
predicate_unmarked: 
  | TRUE
    { Ty.R.P.Bool true }
  | FALSE 
    { Ty.R.P.Bool false }
  | n = NUM 
    { Ty.R.P.Int (Int.of_float n) }
  | v = VAR 
    { Ty.R.P.Var v }
  | l = predicate op = predicate_interp_op r = predicate 
    { Ty.R.P.IntOp (op, l, r) }
  | l = predicate AND r = predicate 
    { Ty.R.P.Conj (l, r) }
  | l = predicate OR r = predicate 
    { Ty.R.P.Disj (l, r) }
  | IF cond = predicate THEN t = predicate ELSE f = predicate 
    { Ty.R.P.IfThen (cond, t, f) }
  | LPAREN p = predicate_unmarked RPAREN 
    { p }

%inline 
predicate_interp_op:
  | EQUAL
    { Ty.R.P.InterpOp.Equal }
  | LESS
    { Ty.R.P.InterpOp.Less } 
  | GREATER
    { Ty.R.P.InterpOp.Greater }
  | PLUS
    { Ty.R.P.InterpOp.Add }
  | MINUS 
    { Ty.R.P.InterpOp.Sub }
  | TIMES 
    { Ty.R.P.InterpOp.Mult }
  | MOD 
    { Ty.R.P.InterpOp.Mod }

(* 
  ---- utils ---- 
 *)
mark_location(X):
    x = X
    { Utils.Location.locate (Utils.Location.from $startpos $endpos) x }

mark_annot_location(X):
    x = X 
    { Ty_template.annotated x (Utils.Location.from $startpos $endpos) }

mark_val_location(X):
    x = X 
    { Ty_template.valstmt x (Utils.Location.from $startpos $endpos) }
%%
