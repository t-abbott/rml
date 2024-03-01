open Z3

let int_sort ctx = Arithmetic.Integer.mk_sort ctx
let bool_sort ctx = Boolean.mk_sort ctx
let mk_int_expr ctx x = Arithmetic.Integer.mk_numeral_i ctx x
let mk_bool_expr ctx b = (if b then Boolean.mk_true else Boolean.mk_false) ctx

let mk_var_expr ctx x sort =
  let x_sym = Z3.Symbol.mk_string ctx x in
  Z3.Expr.mk_const ctx x_sym sort
