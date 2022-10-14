%{
  open Syntax
%}

%token ARROW
%token <Syntax.ident> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS GREATER
%token AND OR
%token IF THEN ELSE
%token FUN IS
%token LPAREN RPAREN
%token LET IN
%token SEMISEMI
%token EOF

%start file
%type <Syntax.program> file

%nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%right ARROW

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

def:
  | LET x = VAR EQUAL e = expr
    { Command.LetDef (x, e) }

expr:
  | e = app_expr
    { e }
  | MINUS n = INT
    { Int (-n) }
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

app_expr:
  | e = simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, e2) }

simple_expr:
  | x = VAR
    { Var x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | LPAREN e = expr RPAREN	
    { e }    

// ty:
//   | TBOOL
//     { TBool }
//   | TINT
//     { TInt }
//   | t1 = ty TARROW t2 = ty
//     { TArrow (t1, t2) }
//   | LPAREN t = ty RPAREN
//     { t }

%%
