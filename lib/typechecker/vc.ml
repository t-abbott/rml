open Typing
open Ast
open Utils
module Ty = Ty_template
module Tree = Lineartree

let impl_constraint (x : Ident_core.t) (ty : Ty.t) (c : Constraint.t) :
    Constraint.t =
  match ty.body with
  | Ty.RBase (base_ty, Some r) ->
      let v = Ident_core.var r.bound_var in
      let p = Predicate.of_ref r |> Predicate.sub v x in
      Constraint.impl x base_ty p c
  | _ -> c

let sub (ctx : Ty.context) (t1 : Ty.t) (t2 : Ty.t) : Constraint.t =
  ignore (t1, t2, ctx);
  failwith "not implemented"

and synth (ctx : Ty.context) (expr : Tree.t) : Constraint.t * Ty.t =
  ignore (ctx, expr);
  failwith "not implemented"

and check (ctx : Ty.context) (expr : Tree.t) (ty : Ty.t) : Constraint.t =
  ignore (ctx, expr, ty);
  failwith "not implemented"
