open Utils
open Typing
open Typing.Ty_template

module Binop = struct
  type t =
    | Equal
    | Less
    | Greater
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | And
    | Or

  let to_string = function
    | Equal -> "="
    | Less -> "<"
    | Greater -> ">"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"

  let to_ident_core op = Ident_core.BuiltinSym (to_string op)

  (* useful aliases *)
  let x, y, z = ("__1", "__2", "__3")

  type ty_builder = ?pred:R.P.t option -> Ident.t -> Ty_template.t

  let mk_equal v p : R.P.t =
    let ul = Location.unlocated in
    let open R.P in
    ul (IntOp (InterpOp.Equal, ul (Var v), p))

  let make_binop_ty mk_s1 mk_s2 (mk_t : ty_builder) mk_p =
    let pred = Some (mk_equal z (mk_p x y)) in
    let t = mk_t ~pred z in
    let s1 = mk_s1 "v" in
    let s2 = mk_s2 "v" in
    let g = builtin (RArrow (y, s2, t)) in
    builtin (RArrow (x, s1, g))

  let signature = function
    | Equal -> make_binop_ty t_num t_num t_bool R.P.p_equal
    | Less -> make_binop_ty t_num t_bool t_bool R.P.p_less
    | Greater -> make_binop_ty t_num t_num t_bool R.P.p_greater
    | Plus -> make_binop_ty t_num t_num t_num R.P.p_add
    | Minus -> make_binop_ty t_num t_num t_num R.P.p_sub
    | Times -> make_binop_ty t_num t_num t_num R.P.p_mult
    | Div -> make_binop_ty t_num t_num t_num R.P.p_div
    | Mod -> make_binop_ty t_num t_num t_num R.P.p_mod
    | And -> make_binop_ty t_num t_num t_num R.P.p_and
    | Or -> make_binop_ty t_num t_num t_num R.P.p_or
end
