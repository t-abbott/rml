%{
  open Ast.Op
  open Ast.Parsetree
  open Typing
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

ty: mark_annot_location(ty_unmarked) { $1 }
ty_val: mark_val_location(ty_unmarked) { $1 }
ty_unmarked:
  | t = ty_basic r = refinement
    { Ty_surface.SBase (t, Some r) }
  | t1 = ty ARROW t2 = ty
    // todo match list of types
    { Ty_surface.SArrow ([t1], t2)  }
  | t = ty_basic  
    { Ty_surface.SBase (t, None) }
  | LPAREN t = ty_unmarked RPAREN
    { t }

ty_basic:
  | TBOOL
    { Base_ty.TBool }
  | TNUM
    { Base_ty.TInt } 

refinement:
  | LBRACKET v = VAR LINE body = refinement_expr RBRACKET
    { Refinement_surface.refinement v body }

(*
    TODO refactor true and false tokens to wrap ocaml bools like integer does
*)

refinement_expr: mark_location(refinement_expr_unmarked) { $1 }
refinement_expr_unmarked:
  | TRUE 
    { Refinement_surface.boolean true }
  | FALSE 
    { Refinement_surface.boolean false }
  | n = NUM 
    { Refinement_surface.number n }
  | l = refinement_expr op = refinement_binop r = refinement_expr
    { Refinement_surface.Binop (op, l, r) }
  | v = VAR
    { Refinement_surface.var v }
  | LPAREN r = refinement_expr_unmarked RPAREN
   { r }

%inline
refinement_binop:
  | LESS
    { Refop.Binop.Less } 
  | GREATER
    { Refop.Binop.Greater }
  | EQUAL
    { Refop.Binop.Equal }
  | AND
    { Refop.Binop.And }
  | OR
    { Refop.Binop.Or }
  | PLUS
    { Refop.Binop.Add }
  | MINUS 
    { Refop.Binop.Sub }
  | MOD 
    { Refop.Binop.Mod }

mark_location(X):
    x = X
    { Utils.Location.locate (Utils.Location.from $startpos $endpos) x }

mark_annot_location(X):
    x = X 
    { Ty_surface.annotated x (Utils.Location.from $startpos $endpos) }

mark_val_location(X):
    x = X 
    { Ty_surface.valstmt x (Utils.Location.from $startpos $endpos) }
%%
