open Z3

let int_sort ctx = Arithmetic.Integer.mk_sort ctx
let bool_sort ctx = Boolean.mk_sort ctx

let mk_int_expr ctx x =
  let x_sym = Symbol.mk_int ctx x in
  Arithmetic.Integer.mk_const ctx x_sym

let mk_bool_expr ctx b = (if b then Boolean.mk_true else Boolean.mk_false) ctx
