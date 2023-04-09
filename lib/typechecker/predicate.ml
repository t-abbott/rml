open Printf
open Typing
open Utils
module Ref = Refinement_core

module InterpOp = struct
  type t = Add | Sub | Mult | Div | Mod | Equal | Less | Greater

  let type_of (_ : t) : Ty_template.t = failwith "not implemented"

  let to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Equal -> "="
    | Less -> "<"
    | Greater -> ">"

  let of_refop = function
    | Refop.Binop.Less -> Some Less
    | Refop.Binop.Greater -> Some Greater
    | Refop.Binop.Equal -> Some Equal
    | Refop.Binop.Add -> Some Add
    | Refop.Binop.Sub -> Some Sub
    | Refop.Binop.Mod -> Some Mod
    | Refop.Binop.And | Refop.Binop.Or -> None
end

type t =
  | Var of Ident_core.t
  | Bool of bool
  | Int of int
  | IntOp of InterpOp.t * t * t
  | Conj of t * t
  | Disj of t * t
  | IfThen of t * t * t

let rec to_string = function
  | Var v -> Ident_core.to_string v
  | Bool b -> Bool.to_string b
  | Int i -> Int.to_string i
  | IntOp (op, l, r) ->
      let l_str = to_string l in
      let r_str = to_string r in
      sprintf "(%s %s %s)" l_str (InterpOp.to_string op) r_str
  | Conj (l, r) -> sprintf "(%s && %s)" (to_string l) (to_string r)
  | Disj (l, r) -> sprintf "(%s || %s)" (to_string l) (to_string r)
  | IfThen (cond, if_t, if_f) ->
      let c_str, t_str, f_str = Misc.proj3 to_string cond if_t if_f in
      sprintf "(if %s then %s else %s)" c_str t_str f_str

let rec of_ref_expr (r : Ref.t_expr) : t =
  match r.body with
  | Ref.Var v -> Var (Ident_core.var v)
  | Ref.Const (Constant.Boolean b) -> Bool b
  | Ref.Const (Constant.Number n) ->
      if not (Float.is_integer n) then
        failwith "ERROR refined num isn't int TODO fix later"
      else Int (Int.of_float n)
  | Ref.Binop (op, l, r) -> (
      let l_p, r_p = Misc.proj2 of_ref_expr l r in
      match op with
      | Refop.Binop.And -> Conj (l_p, r_p)
      | Refop.Binop.Or -> Disj (l_p, r_p)
      | _ -> (
          match InterpOp.of_refop op with
          | Some intop -> IntOp (intop, l_p, r_p)
          | None -> failwith "unreachable"))
  | Ref.IfThen (cond, if_t, if_f) ->
      let p_cond, p_t, p_f = Misc.proj3 of_ref_expr cond if_t if_f in
      IfThen (p_cond, p_t, p_f)

let of_ref (r : Ref.t) = of_ref_expr r.expr

(**
  Substitutes [v] for [x] in [p]
*)
let rec sub (v : Ident_core.t) (x : Ident_core.t) (p : t) =
  let f = sub v x in
  match p with
  | Var u -> Var (if u <> v then u else x)
  | IntOp (op, l, r) -> IntOp (op, f l, f r)
  | Conj (l, r) -> Conj (f l, f r)
  | Disj (l, r) -> Disj (f l, f r)
  | IfThen (cond, if_t, if_f) -> IfThen (f cond, f if_t, f if_f)
  | _ -> p
