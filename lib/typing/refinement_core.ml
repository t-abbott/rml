open Printf
open Utils
open Refop
module Loc = Location

type t_expr = t_expr_body Location.located

and t_expr_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t_expr * t_expr
  | IfThen of t_expr * t_expr * t_expr

type t = { bound_var : Ident.t; expr : t_expr }

let rec equal_body (r1 : t_expr) (r2 : t_expr) =
  match (r1.body, r2.body) with
  | Var v1, Var v2 -> v1 = v2
  | Const c1, Const c2 -> c1 = c2
  | Binop (op1, l1, r1), Binop (op2, l2, r2) ->
      op1 = op2 && equal_body l1 l2 && equal_body r1 r2
  | IfThen (cond1, t1, f1), IfThen (cond2, t2, f2) ->
      equal_body cond1 cond2 && equal_body t1 t2 && equal_body f1 f2
  | _ -> false

let equal r1 r2 = r1.bound_var = r2.bound_var && equal_body r1.expr r2.expr

let rec expr_to_string (ref : t_expr) =
  match ref.body with
  | Var v -> v
  | Const c -> Constant.to_string c
  | Binop (op, l, r) ->
      sprintf "(%s %s %s)" (expr_to_string l) (Binop.to_string op)
        (expr_to_string r)
  | IfThen (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s)" (expr_to_string cond)
        (expr_to_string if_t) (expr_to_string if_f)

let to_string (ref : t) = ref.bound_var ^ " | " ^ expr_to_string ref.expr
let boolean b = Const (Constant.Boolean b)
let number n = Const (Constant.Number n)
let var v = Var v
