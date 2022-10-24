%{
  open Parsetree
  open Rml.Types
%}

%token TINT
%token TBOOL
%token COLON

%token ARROW
%token <Parsetree.ident> VAR

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

%token SEMISEMI
%token EOF

%start file
%type <Parsetree.program> file


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
    { Binop (Op.Plus, e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Binop (Op.Minus, e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Binop (Op.Times, e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { Binop(Op.Equal, e1, e2) }
  | e1 = expr LESS e2 = expr
    { Binop (Op.Less, e1, e2) }
  | e1 = expr GREATER e2 = expr 
    { Binop (Op.Greater, e1, e2) }
  | e1 = expr AND e2 = expr
    { Binop (Op.And, e1, e2) }
  | e1 = expr OR e2 = expr
    { Binop (Op.Or, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN arg = VAR ARROW body = expr 
    { Fun (arg, body) }
  | LET name = VAR EQUAL e1 = expr IN e2 = expr
    { LetIn (name, e1, e2) }

app_expr: mark_location(app_expr_unmarked) { $1 }
app_expr_unmarked:
  | e = simple_expr_unmarked
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, e2) }

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
  | LPAREN e = expr COLON t = ty RPAREN
    { Annotated (e, t) }
  | LPAREN e = expr_unmarked RPAREN	
    { e }    

ty: mark_location(ty_unmarked) { $1 }
ty_unmarked:
  | TBOOL
    { Ty.TBool }
  | TINT
    { Ty.TInt }
  | t1 = ty ARROW t2 = ty
    { Ty.TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }

mark_location(X):
    x = X
    { Location.locate (Location.from $startpos $endpos) x }

%%
