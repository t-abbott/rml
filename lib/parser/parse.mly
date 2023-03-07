%{
  open Ast.Op
  open Ast.Parsetree
  open Typing
%}

%token TINT
%token TBOOL
%token COLON
%token VAL

%token ARROW
%token <Ast.Ident.t> VAR

%token <int> INT
%token TRUE FALSE

%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS GREATER
%token AND OR

%token IF THEN ELSE
%token FUN
%token LPAREN RPAREN
%token LET IN

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
  | LET x = VAR COLON t = ty EQUAL e = expr
    { LetDef (x, Some t, e) }
  | LET x = VAR EQUAL e = expr
    { LetDef (x, None, e) }
  | VAL x = VAR COLON t = ty
    { ValDef (x, t) }

topexpr: mark_location(topexpr_unmarked) { $1 }
topexpr_unmarked:
| e = expr
    { Expr e }

expr: mark_location(expr_unmarked) { $1 }
expr_unmarked:
  | e = app_expr_unmarked
    { e }
  | MINUS n = INT
    { Integer (-n) }
  | e1 = expr PLUS e2 = expr	
    { Binop (Binop.Plus, e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Binop (Binop.Minus, e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Binop (Binop.Times, e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { Binop(Binop.Equal, e1, e2) }
  | e1 = expr LESS e2 = expr
    { Binop (Binop.Less, e1, e2) }
  | e1 = expr GREATER e2 = expr 
    { Binop (Binop.Greater, e1, e2) }
  | e1 = expr AND e2 = expr
    { Binop (Binop.And, e1, e2) }
  | e1 = expr OR e2 = expr
    { Binop (Binop.Or, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN arg = expr ARROW body = expr 
    { Fun ([arg], body) }
  | LET name = expr EQUAL e1 = expr IN e2 = expr
    { LetIn (name, e1, e2) }

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
  | n = INT
    { (Integer n) }
  | e = expr COLON t = ty
    { Annotated (e, t) }
  | LPAREN e = expr_unmarked RPAREN	
    { e }    

ty: mark_type_location(ty_unmarked) { $1 }
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
  | TINT
    { Base_ty.TInt } 

refinement:
  | LBRACKET v = VAR LINE body = refinement_expr RBRACKET
    {
        if v <> "v" then
            let loc = Utils.Location.from $startpos $endpos in
            let msg = Printf.sprintf "refinements must bind the variable 'v', found '%s'" v in
            raise (Errors.ParseError (msg, loc))
        else
            body
    }

(*
    TODO refactor true and false tokens to wrap ocaml bools like integer does
*)

refinement_expr: mark_location(refinement_expr_unmarked) { $1 }
refinement_expr_unmarked:
  | TRUE 
    { Refinement_surface.boolean true }
  | FALSE 
    { Refinement_surface.boolean false }
  | i = INT 
    { Refinement_surface.number i }
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

mark_location(X):
    x = X
    { Utils.Location.locate (Utils.Location.from $startpos $endpos) x }

// TODO: avoid this
mark_type_location(X):
    x = X 
    { Ty_surface.annotated x (Utils.Location.from $startpos $endpos) }

%%
