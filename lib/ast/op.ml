open Utils
open Typing
open Typing.Ty_template

(* let t_int = unrefined Base_ty.TInt ~source:Builtin *)
(* let t_bool = unrefined Base_ty.TBool ~source:Builtin *)

(* let int_int_int = builtin (RArrow ([ t_int; t_int ], t_int))
   let int_int_bool = builtin (RArrow ([ t_int; t_int ], t_bool))
   let bool_bool_bool = builtin (RArrow ([ t_bool; t_bool ], t_bool)) *)

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

  (* useful aliases *)
  let x, y = ("__param1", "__param2")

  type ty_builder = ?pred:R.P.t option -> Ident.t -> Ty_template.t

  let make_binop_ty mk_s1 mk_s2 (mk_t : ty_builder) mk_p =
    let pred = Some (mk_p x y) in
    let t = mk_t ~pred "v" in
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
