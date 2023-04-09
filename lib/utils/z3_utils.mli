val int_sort : Z3.context -> Z3.Sort.sort
(**
  Constructs an integer sort.    
*)

val bool_sort : Z3.context -> Z3.Sort.sort
(**
  Constructs a boolean sort.    
*)

val mk_int_expr : Z3.context -> int -> Z3.Expr.expr
(**
  [mk_int_expr ctx x] constructs a constant int expression
  of value [x] wrt the context [ctx].
*)

val mk_bool_expr : Z3.context -> bool -> Z3.Expr.expr
(**
  [mk_bool_expr ctx b] constructs a context bool expression
  of arity [b] wrt the context [ctx].    
*)
