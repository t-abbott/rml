open Printf
open Utils
module C = Constraint
module Ty = Typing.Ty_template
module P = Ty.R.P
module Ctx = Context.Make (Ident)

type sort_ctx = Z3.Sort.sort Ctx.t
type result = SAT | UNSAT | UNKNOWN

let logic = "QF_UFLIA"
let z3_config = [ ("model", "true"); ("proof", "true") ]
let z3_ctx = Z3.mk_context z3_config

let intop_mk_fn op =
  let uncurry f ctx x y = f ctx [ x; y ] in

  let open P.InterpOp in
  match op with
  | Add -> uncurry Z3.Arithmetic.mk_add
  | Sub -> uncurry Z3.Arithmetic.mk_sub
  | Mult -> uncurry Z3.Arithmetic.mk_mul
  | Div -> Z3.Arithmetic.mk_div
  | Equal -> Z3.Boolean.mk_eq
  | Less -> Z3.Arithmetic.mk_lt
  | Greater -> Z3.Arithmetic.mk_gt
  | Mod -> Z3.Arithmetic.Integer.mk_mod

let sort_of_ty = function
  | Typing.Base_ty.TInt -> Z3_utils.int_sort z3_ctx
  | Typing.Base_ty.TBool -> Z3_utils.bool_sort z3_ctx

let rec encode_predicate (ctx : sort_ctx) (p : P.t) =
  match p.body with
  | P.Var v -> (
      match Ctx.find v ctx with
      | Some sort -> Z3_utils.mk_var_expr z3_ctx v sort
      | None -> failwith ("uh oh don't know the sort of var " ^ v))
  | P.Bool b -> Z3_utils.mk_bool_expr z3_ctx b
  | P.Int i -> Z3_utils.mk_int_expr z3_ctx i
  | P.IntOp (op, l, r) ->
      let mk_fn = intop_mk_fn op in
      let l_expr, r_expr = Misc.proj2 (encode_predicate ctx) l r in
      mk_fn z3_ctx l_expr r_expr
  | P.IfThen (cond, if_t, if_f) ->
      let cond_expr = encode_predicate ctx cond in
      let if_t_expr, if_f_expr = Misc.proj2 (encode_predicate ctx) if_t if_f in
      Z3.Boolean.mk_ite z3_ctx cond_expr if_t_expr if_f_expr
  | P.Conj (p, q) ->
      let p_expr, q_expr = Misc.proj2 (encode_predicate ctx) p q in
      Z3.Boolean.mk_and z3_ctx [ p_expr; q_expr ]
  | P.Disj (p, q) ->
      let p_expr, q_expr = Misc.proj2 (encode_predicate ctx) p q in
      Z3.Boolean.mk_or z3_ctx [ p_expr; q_expr ]

let rec encode_constraint (ctx : sort_ctx) (c : C.t) =
  match c with
  | C.Pred p -> encode_predicate ctx p
  | C.Conj (c1, c2) ->
      let e1, e2 = Misc.proj2 (encode_constraint ctx) c1 c2 in
      Z3.Boolean.mk_and z3_ctx [ e1; e2 ]
  | C.Impl { x; base; p; c } ->
      let sort = sort_of_ty base in
      let ctx' = Ctx.extend x sort ctx in
      let p_expr = encode_predicate ctx' p in
      let c_expr = encode_constraint ctx' c in
      let expr = Z3.Boolean.mk_implies z3_ctx p_expr c_expr in
      encode_forall x sort expr

and encode_forall x sort expr =
  let open Z3.Quantifier in
  let x_sym = Z3.Symbol.mk_string z3_ctx x in
  let x_expr = Z3.Expr.mk_const z3_ctx x_sym sort in
  mk_forall_const z3_ctx [ x_expr ] expr (Some 1) [] [] None None
  |> expr_of_quantifier

let solve c =
  let c_expr = encode_constraint [] c in

  Log.log (sprintf "got vc:\n\n%s\n" (C.to_string c));
  Log.log (sprintf "generated encoding:\n\n%s\n" (Z3.Expr.to_string c_expr));
  Log.log
    (sprintf "simplified to\n\n%s\n"
       (Z3.Expr.simplify c_expr None |> Z3.Expr.to_string));

  let solver = Z3.Solver.mk_solver_s z3_ctx logic in
  Z3.Solver.add solver [ c_expr ];

  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE -> SAT
  | Z3.Solver.UNSATISFIABLE -> UNSAT
  | Z3.Solver.UNKNOWN -> UNKNOWN
