open Printf
open Utils
open Refop

type t_expr = t_expr_body Location.located

and t_expr_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t_expr * t_expr
  | IfThen of t_expr * t_expr * t_expr

type t = { bound_var : Ident.t; expr : t_expr }

let rec expr_to_string (r : t_expr) =
  match r.body with
  | Var v -> v
  | Const c -> Constant.to_string c
  | Binop (op, l, r) ->
      sprintf "%s %s %s" (expr_to_string l) (Binop.to_string op)
        (expr_to_string r)
  | IfThen (cond, if_t, if_f) ->
      sprintf "if %s then %s else %s" (expr_to_string cond)
        (expr_to_string if_t) (expr_to_string if_f)

let to_string (r : t) = r.bound_var ^ " | " ^ expr_to_string r.expr

(*
  The following functions are convenient aliases, mostly for
  use in the parser (lib/parser/parse.mly).   
*)

let var v = Var v
let boolean b = Const (Constant.Boolean b)
let number n = Const (Constant.Number n)
let refinement bound_var expr = { bound_var; expr }
