open Base
open Typing
open Ast
open Errors
open Utils
module Tree = Lineartree
module Ty = Ty_template
module P = Predicate.Make (Ident)
module C = Constraint

(* ctx is a typing context, so we need to track the type of every variable we come across *)

let unwrap = function Some p -> p | None -> P.p_true

(**
  TODO figure out how to track the location an error happened at    
*)
let error = function
  | `MismatchedTypes ->
      let msg = "" in
      raise (RefinementError (msg, Location.Nowhere))
  | `ExpectedBase loc ->
      let msg = "" in
      raise (RefinementError (msg, loc))
  | `ExpectedArrow loc ->
      let msg = "" in
      raise (RefinementError (msg, loc))
  | `MismatchedNames (x_expr, x_ty, loc) ->
      ignore (x_expr, x_ty);
      let msg = "" in
      raise (RefinementError (msg, loc))

let impl_constraint (x : Ident.t) (ty : Ty.t) (c : C.t) : Constraint.t =
  ignore (x, ty, c);
  match ty.body with
  | Ty.RBase { vname; base; pred } ->
      let p = unwrap pred in
      C.impl vname base p c
  | Ty.RArrow _ -> c

let rec sub (t1 : Ty.t) (t2 : Ty.t) : Constraint.t =
  match (t1.body, t2.body) with
  | RBase r1, RBase r2 ->
      let v1, v2 = (r1.vname, r2.vname) in
      let b = r1.base in

      let p1, p2 = (unwrap r1.pred, unwrap r2.pred) in
      let p2_sub = C.pred (P.sub v2 v1 p2) in

      C.impl v1 b p1 p2_sub
  | RArrow (x, s1, t1), RArrow (y, s2, t2) ->
      let t1' = Ty.sub x y t1 in

      let c_i = sub s1 s2 in
      let c_o = sub t1' t2 in
      let impl = impl_constraint y s2 c_o in

      C.conj c_i impl
  | _ -> error `MismatchedTypes

(* TODO how tf do you handle binops *)
and synth (ctx : Ty.context) (expr : Tree.t) : Constraint.t * Ty.t =
  match expr.body with
  | Tree.CExpr ce -> synth_cexpr ctx ce
  | _ -> (check ctx expr expr.ty, expr.ty)

and synth_cexpr (ctx : Ty.context) (cexpr : Tree.cexpr) : C.t * Ty.t =
  let open Ty in
  match cexpr.body with
  | Tree.CApply (e, y) ->
      (* generate a type and vc for [e] (must be a function) *)
      let c_arg, (x, s, t) =
        match synth_aexpr ctx e with
        | c, { body = RArrow (x, s, t); _ } -> (c, (x, s, t))
        | _ -> failwith "can't apply to an arrow type"
      in

      (* generate vs s.t. [y] has satisfies the parameter type [s] *)
      let c_app = check_aexpr ctx y s in

      (* here's some bs: [y] is an arbitrary term but he glosses over that in
         the description of the algorithm
      *)
      let y_imm =
        match y.body with
        | Tree.ANumber n -> R.P.mk_int (Int.of_float n)
        | Tree.ABoolean b -> R.P.mk_bool b
        | Tree.AVar v_core ->
            (* recover the original name of the variable being projected into a type *)
            let v_orig =
              match v_core with
              | Ident_core.Var v -> v
              | Ident_core.Sym _ -> Ident_core.to_string v_core
            in
            R.P.mk_var v_orig
        | Tree.ALambda _ -> failwith "can't substitute a function"
      in
      let t' = Ty.sub_term x y_imm t in

      (C.conj c_arg c_app, t')
  | Tree.CAexpr ae -> synth_aexpr ctx ae
  | _ ->
      (* otherwise check the stated type is valid *)
      (check_cexpr ctx cexpr cexpr.ty, cexpr.ty)

and synth_aexpr (ctx : Ty.context) (aexpr : Tree.aexpr) =
  match aexpr.body with
  | Tree.AVar v -> (
      match Context.find (Ident_core.to_string v) ctx with
      | Some ty -> (C.c_true, ty)
      | None -> failwith "unreachable")
  | Tree.ANumber n ->
      (* TODO turn floats back into ints *)
      let i = Int.of_float n in
      (C.c_true, Ty.prim_int i)
  | Tree.ABoolean b -> (C.c_true, Ty.prim_bool b)
  | _ ->
      (* otherwise check that the stated type is valid *)
      (check_aexpr ctx aexpr aexpr.ty, aexpr.ty)

(* checks that an expression [expr] satisfies the type [ty] *)
and check (ctx : Ty.context) (expr : Tree.t) (ty : Ty.t) : Constraint.t =
  match expr.body with
  | Tree.Let (x_id, value, body) ->
      let x = Ident_core.to_string x_id in

      (* generate vc and type for [value] *)
      let c_value, t_value = synth_cexpr ctx value in

      (* generate vc and type for [body] *)
      let ctx' = Context.extend x t_value ctx in
      let c_body = check ctx' body ty in

      C.conj c_value (impl_constraint x t_value c_body)
  | Tree.CExpr ce -> check_cexpr ctx ce ce.ty

and check_cexpr ctx cexpr ty =
  match cexpr.body with
  | Tree.CBinop (_, _, _) -> failwith "binops not supported"
  | Tree.CAexpr ae -> check_aexpr ctx ae ae.ty
  | _ ->
      let c_synth, s = synth_cexpr ctx cexpr in
      let c_sub = sub s ty in
      C.conj c_synth c_sub

and check_aexpr ctx aexpr ty =
  match aexpr.body with
  | Tree.ALambda (arg_id, body) ->
      let arg = Ident_core.orig arg_id in

      let x, s, t =
        match ty.body with
        | RArrow (x, s, t) ->
            if String.equal x arg then (x, s, t)
            else error (`MismatchedNames (x, arg, aexpr.loc))
        | _ -> error (`ExpectedArrow aexpr.loc)
      in

      (* jsdkf *)
      let ctx' = Context.extend x s ctx in
      let c = check ctx' body t in

      impl_constraint x s c
  | _ ->
      let c_synth, s = synth_aexpr ctx aexpr in
      let c_sub = sub s ty in
      C.conj c_synth c_sub

let check_command ctx (cmd : Tree.command) : C.t * Ty.context =
  match cmd with
  | Tree.LetDef (x, body) ->
      let c = check ctx body body.ty in
      let ctx' = Context.extend (Ident_core.to_string x) body.ty ctx in
      (c, ctx')
  | Tree.Expr e -> (check ctx e e.ty, ctx)

let rec check_program ctx prog : C.t list =
  match prog with
  | cmd :: rest ->
      let c, ctx' = check_command ctx cmd in
      c :: check_program ctx' rest
  | [] -> []
