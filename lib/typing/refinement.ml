open Printf
open Utils
module Loc = Location

module Binop = struct
  type t = Less | Greater | Equal | And | Or | Add

  let to_string = function
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "="
    | And -> "&&"
    | Or -> "||"
    | Add -> "+"
end

type t = t_body Location.located

and t_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t * t
  | IfThen of t * t * t

let equal x y = x = y

let rec to_string (ref : t) =
  match ref.body with
  | Var v -> v
  | Const c -> Constant.to_string c
  | Binop (op, l, r) ->
      sprintf "(%s %s %s)" (to_string l) (Binop.to_string op) (to_string r)
  | IfThen (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t)
        (to_string if_f)

let boolean b = Const (Constant.Boolean b)
let number n = Const (Constant.Integer n)
let var v = Var v
