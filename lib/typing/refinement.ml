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

let equal x y = x = y

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
let number n = Const (Constant.Integer n)
let var v = Var v
