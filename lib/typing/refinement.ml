open Printf
open Utils

module Binop = struct
  type t = Less | Greater | Equal | And | Or | Add | Sub

  let to_string = function
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "="
    | And -> "&&"
    | Or -> "||"
    | Add -> "+"
    | Sub -> "-"
end

(*
  TODO: read up on how interpreted and uniterpreted operators
  are implemented. For now I'll just stick with propositional
  logic (also interesting that Jhala doesn't include implies
  in his predicates).   
*)

type t =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t * t
  | IfThen of t * t * t

let equal x y = x = y

let rec to_string = function
  | Var v -> v
  | Const c -> Constant.to_string c
  | Binop (op, l, r) ->
      sprintf "(%s %s %s)" (to_string l) (Binop.to_string op) (to_string r)
  | IfThen (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t)
        (to_string if_f)

(** Alias for [Const (Constant.Boolean ...)] *)
let boolean b = Const (Constant.Boolean b)

let var v = Var v
