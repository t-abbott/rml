open Printf
open Utils
open Refop

type t = t_body Location.located

and t_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t * t
  | IfThen of t * t * t

let rec to_string (r : t) =
  match r.body with
  | Var v -> v
  | Const c -> Constant.to_string c
  | Binop (op, l, r) ->
      sprintf "%s %s %s" (to_string l) (Binop.to_string op) (to_string r)
  | IfThen (cond, if_t, if_f) ->
      sprintf "if %s then %s else %s" (to_string cond) (to_string if_t)
        (to_string if_f)

let boolean b = Const (Constant.Boolean b)
