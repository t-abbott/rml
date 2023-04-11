open Typing
open Ast
open Utils
module Ty = Ty_template
module Tree = Lineartree
module P = Predicate.Make (Ident_core)

let impl_constraint (x : Ident_core.t) (ty : Ty.t) (c : Constraint.t) :
    Constraint.t =
  ignore (x, ty, c);
  match ty.body with
  | Ty.RBase (_, Some _) -> failwith "not implemented"
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
