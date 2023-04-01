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

let rec of_surface_expr (r_expr : Refinement_surface.t_expr) : t_expr =
  let loc = r_expr.loc in
  let body =
    match r_expr.body with
    | Refinement_surface.Var v -> Var v
    | Refinement_surface.Const c -> Const c
    | Refinement_surface.Binop (op, l, r) ->
        Binop (op, of_surface_expr l, of_surface_expr r)
    | Refinement_surface.IfThen (cond, if_t, if_f) ->
        IfThen (of_surface_expr cond, of_surface_expr if_t, of_surface_expr if_f)
  in
  { body; loc }

let of_surface (r_surface : Refinement_surface.t) : t =
  { bound_var = r_surface.bound_var; expr = of_surface_expr r_surface.expr }
