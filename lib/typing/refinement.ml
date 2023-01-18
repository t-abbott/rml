open Printf
open Utils
open Refop
module Loc = Location

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

let rec of_surface (r_surface : Refinement_surface.t) : t =
  let loc = r_surface.loc in
  let body =
    match r_surface.body with
    | Refinement_surface.Var v -> Var v
    | Refinement_surface.Const c -> Const c
    | Refinement_surface.Binop (op, l, r) ->
        Binop (op, of_surface l, of_surface r)
    | Refinement_surface.IfThen (cond, if_t, if_f) ->
        IfThen (of_surface cond, of_surface if_t, of_surface if_f)
  in
  { body; loc }
