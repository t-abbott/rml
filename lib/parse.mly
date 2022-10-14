%{
    open Syntax
%}

// Code structure
%token LPAREN RPAREN
%token SEMISEMI
// %token SEMI
%token EOF

// Literals
%token <string> IDENT
%token TRUE FALSE
%token <int> INT

// Operators
%token EQUALS NEQUALS
%token PLUS MINUS
%token LESS GREATER
%token MULT DIV
%token AND OR
// %token NOT

// Types
// %token COLON
// %token TBOOL
// %token TINT

// Terms
%token FUN ARROW
%token LET IN
%token IF THEN ELSE

%start file
%type <Syntax.program> file

%nonassoc ELSE
%nonassoc EQUALS LESS
%left PLUS MINUS
%left MULT
%right ARROW

%%

// https://journal.stuffwithstuff.com/2008/12/28/fixing-ambiguities-in-yacc/ 

file:
| EOF
    { [] }
| d = letdef EOF
    { [d] }
| d = letdef SEMISEMI lst = file
    { d :: lst }

letdef:
| LET name = IDENT EQUALS e = expr
    { Defn.LetDefn (name, e) }

expr:
| e = expr_app
    { e }
| LET name = IDENT EQUALS e1 = expr IN e2 = expr
    { LetIn (name, e1, e2) }
| FUN arg = IDENT ARROW body = expr 
    { Fun (arg, body) } 
| IF cond = expr THEN if_t = expr ELSE if_f = expr 
    { IfThen (cond, if_t, if_f) }

// TODO: clean this up with Binop.of_string
| l = expr EQUALS r = expr
    { Binop (Op.Equals, l, r) }
| l = expr NEQUALS r = expr 
    { Binop (Op.Nequals, l, r) }
| l = expr LESS r = expr 
    { Binop (Op.Less, l, r) }
| l = expr GREATER r = expr
    { Binop (Op.Greater, l, r) }
| l = expr PLUS r = expr 
    { Binop (Op.Add, l, r) }
| l = expr MINUS r = expr
    { Binop (Op.Sub, l, r) }
| l = expr MULT r = expr
    { Binop (Op.Mult, l, r) }
| l = expr DIV r = expr
    { Binop (Op.Div, l, r) }
| l = expr AND r = expr 
    { Binop (Op.And, l, r) } 
| l = expr OR r = expr 
    { Binop (Op.Or, l, r) }

expr_app:
| e = expr_base
    { e }
| e1 = expr_app e2 = expr_base
    { App (e1, e2) }

expr_base:
| TRUE
    { True }
| FALSE 
    { False }
| i = INT 
    { Int i }
| name = IDENT 
    { Var name }
| LPAREN e = expr RPAREN
    { e }

