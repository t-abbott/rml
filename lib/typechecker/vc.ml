open Base
open Printf
open Typing
open Ast
open Errors
open Utils
module Tree = Lineartree
module Ty = Ty_template
module P = Predicate.Make (Ident)
module C = Constraint

(*
  NOTE:
    - cycle when checking if statements   
    - how to resolve? look at chapter 2
    - fill_cexpr_refinements is probably wrong. I don't know how to handle if statements
*)

(**
  TODO figure out how to track the location an error happened at    
*)
let error = function
  | `MismatchedTypes ->
      let msg = "mismatched types" in
      raise (RefinementError (msg, Location.Nowhere))
  | `ExpectedBase loc ->
      let msg = "expected base" in
      raise (RefinementError (msg, loc))
  | `ExpectedArrow loc ->
      let msg = "expected arrow" in
      raise (RefinementError (msg, loc))
  | `MismatchedNames (arg_ty, arg_expr, loc) ->
      let msg =
        sprintf
          "mismatched function argument names - type declares '%s' but \
           function definition uses '%s'"
          arg_ty arg_expr
      in
      raise (RefinementError (msg, loc))
  | `MissingRefinement ->
      let msg =
        "found an expression with a missing refinement (internal error - this \
         shouldn't happen)"
      in
      raise (RefinementError (msg, Location.Nowhere))

let unwrap (p : Ty.R.P.t option) =
  match p with Some p -> p | None -> error `MissingRefinement

let impl_constraint (x : Ident.t) (ty : Ty.t) (c : C.t) : Constraint.t =
  (* not sure why we ignore it lol *)
  ignore x;
  Stdlib.print_endline ("impl constraint x: " ^ x);
  match ty.body with
  | Ty.RBase { vname; base; pred = Some p } ->
      let p' = P.sub vname x p in
      C.impl x base p' c
  | Ty.RBase { pred = None; _ } -> error `MissingRefinement
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

(* FIXME: argument name substitution isn't working in [CApply] from the looks of it *)
and synth_cexpr (ctx : Ty.context) (cexpr : Tree.cexpr) : C.t * Ty.t =
  let open Ty in
  match cexpr.body with
  | Tree.CApply (e, y) ->
      (* generate a type and vc for [e] (must be a function) *)
      let c_fn, (x, s, t) =
        match synth_aexpr ctx e with
        | c, { body = RArrow (x, s, t); _ } -> (c, (x, s, t))
        | _ -> failwith "can't apply to an arrow type"
      in

      (* generate vs s.t. [y] has satisfies the parameter type [s] *)
      let c_arg = check_aexpr ctx y s in

      (* here's some bs: [y] is an arbitrary term but he glosses over that in
         the description of the algorithm
      *)
      let y_imm =
        match y.body with
        | Tree.ANumber n -> R.P.mk_int (Int.of_float n)
        | Tree.ABoolean b -> R.P.mk_bool b
        | Tree.AVar v_core ->
            (* recover the original name of the variable being projected into a type *)
            let v_orig = Ident_core.orig v_core in
            R.P.mk_var v_orig
        | Tree.ALambda _ -> failwith "can't substitute a function"
      in

      let t' = Ty.sub_term x y_imm t in

      (C.conj c_fn c_arg, t')
  | Tree.CAexpr ae -> synth_aexpr ctx ae
  | _ ->
      (* otherwise check the stated type is valid *)
      (check_cexpr ctx cexpr cexpr.ty, cexpr.ty)

and synth_aexpr (ctx : Ty.context) (aexpr : Tree.aexpr) =
  match aexpr.body with
  | Tree.AVar v -> (
      match v with
      | Ident_core.BuiltinSym s ->
          let op = Op.Binop.of_string s in
          let op_ty = Ty.ty_of_op op in
          (C.c_true, op_ty)
      | _ -> (
          match Context.find (Ident_core.to_string v) ctx with
          | Some ty -> (C.c_true, ty)
          | None ->
              let msg =
                sprintf "found var not in environment: %s"
                  (Ident_core.to_string v)
              in
              failwith msg))
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
  let c_check =
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
  in
  c_check

and check_cexpr ctx cexpr ty =
  match cexpr.body with
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

      (* do I need to rename [x] for [arg] in s/c? TODO fix *)

      (* jsdkf *)
      let ctx' = Context.extend Ident_core.(var x |> to_string) s ctx in
      let c = check ctx' body t in

      impl_constraint x s c
  | _ ->
      let c_synth, s = synth_aexpr ctx aexpr in
      let c_sub = sub s ty in
      C.conj c_synth c_sub

(**
  [fill_refinements ctx e] returns a copy of [e] where all empty refinements
  (denoted by [None]) have been inferred alongside a VC that implies the validity
  of the inferred refinements.

  Note: this only works under the assumption that the only unrefined terms we get
  were produced by splitting up a multiply-applied function.
*)
let rec fill_refinements ctx (expr : Tree.t) =
  match expr.body with
  | Tree.Let (x, value, body) ->
      (* fill the types of any missing refinements in [value] *)
      let c_value, value' = fill_cexpr_refinements ctx value in
      let value' : Tree.cexpr_body Tree.astnode = value' in
      let ctx' = Context.extend (Ident_core.to_string x) value'.ty ctx in

      (* fill the types of any missing refinements in [body] *)
      let c_body, body' = fill_refinements ctx' body in

      ( C.conj c_value c_body,
        Tree.(node_of (Let (x, value', body')) body'.ty expr.loc) )
  | Tree.CExpr ce ->
      let c, ce' = fill_cexpr_refinements ctx ce in

      (c, Tree.(node_of (CExpr ce') ce'.ty ce.loc))

and fill_cexpr_refinements ctx (ce : Tree.cexpr) =
  (* TODO - this is WRONG I think. the refinements should match right? idk how else to square it.*)
  match ce.body with
  | Tree.CIf (cond, if_t, if_f) ->
      let (c_cond, cond'), (c_t, if_t'), (c_f, if_f') =
        Misc.proj3 (fill_aexpr_refinements ctx) cond if_t if_f
      in

      (* conj all branch conditions (fine because AND is associative) *)
      let c = C.(conj c_cond (conj c_t c_f)) in

      (* what's the type of the overall expression then? THIS IS WRONG but idk how to fix it... *)
      (c, Tree.(node_of (CIf (cond', if_t', if_f')) ce.ty ce.loc))
  | Tree.CApply (e, y) ->
      let c_e, e' = fill_aexpr_refinements ctx e in
      let c_y, y' = fill_aexpr_refinements ctx y in
      let ce' = Tree.(node_of (CApply (e', y')) ce.ty ce.loc) in

      (* get the type of [e] (should be a function) *)
      let x, _, t =
        match e'.ty.body with
        | Ty.RArrow (x, s, t) -> (x, s, t)
        | _ -> failwith "can't apply to an arrow type"
      in

      (* manually sub the refinement argument in. Is this sound? Who knows *)
      let y_imm =
        match y.body with
        | Tree.ANumber n -> Ty.R.P.mk_int (Int.of_float n)
        | Tree.ABoolean b -> Ty.R.P.mk_bool b
        | Tree.AVar v_core ->
            (* recover the original name of the variable being projected into a type *)
            let v_orig = Ident_core.orig v_core in
            Ty.R.P.mk_var v_orig
        | Tree.ALambda _ -> failwith "can't substitute a function"
      in

      let ty = Ty.sub_term x y_imm t in
      (C.conj c_e c_y, { ce' with ty })
  | Tree.CAexpr ae ->
      let c, ae' = fill_aexpr_refinements ctx ae in
      (c, Tree.(node_of (CAexpr ae') ae'.ty ae'.loc))

and fill_aexpr_refinements ctx ae =
  match ae.body with
  | Tree.AVar v -> (
      match v with
      | Ident_core.BuiltinSym s ->
          let op = Op.Binop.of_string s in
          let ty = Ty.ty_of_op op in
          (C.c_true, { ae with ty })
      | _ -> (
          match Context.find (Ident_core.to_string v) ctx with
          | Some ty -> (C.c_true, { ae with ty })
          | None ->
              let msg =
                sprintf "found var not in environment: %s"
                  (Ident_core.to_string v)
              in
              failwith msg))
  | Tree.ANumber n ->
      let i = Int.of_float n in
      let ty = Ty.prim_int i in
      (C.c_true, { ae with ty })
  | Tree.ABoolean b ->
      let ty = Ty.prim_bool b in
      (C.c_true, { ae with ty })
  | Tree.ALambda (arg_id, body) ->
      let arg = Ident_core.orig arg_id in

      let x, s, _ =
        match ae.ty.body with
        | Ty.RArrow (x, s, t) ->
            if String.equal x arg then (x, s, t)
            else error (`MismatchedNames (x, arg, ae.loc))
        | _ -> error (`ExpectedArrow ae.loc)
      in

      (* fill in the refinements in [body] *)
      let ctx' = Context.extend Ident_core.(var x |> to_string) s ctx in
      let c_body, body' = fill_refinements ctx' body in

      (* we don't need to change the type of [body] since function types must be specified *)
      (c_body, { ae with body = Tree.ALambda (arg_id, body') })

let check_command ctx (cmd : Tree.command) : C.t * Ty.context =
  match cmd with
  | Tree.LetDef (x, body) ->
      (* infer any missing refinements on body *)
      let c_inferred, body' = fill_refinements ctx body in

      Stdlib.print_endline
        (sprintf "\nAfter filling missing refinements:\n%s"
           Tree.(LetDef (x, body') |> command_to_string));

      let c = check ctx body' body'.ty in
      let ctx' = Context.extend (Ident_core.to_string x) body'.ty ctx in
      (C.conj c_inferred c, ctx')
  | Tree.Expr e ->
      let c_inferred, e' = fill_refinements ctx e in
      Stdlib.print_endline
        (sprintf "\nAfter filling missing refinements:\n%s" (Tree.to_string e'));

      let c, _ = (check ctx e' e'.ty, ctx) in

      (C.conj c c_inferred, ctx)

let rec check_program ctx prog : C.t list =
  match prog with
  | cmd :: rest ->
      let c, ctx' = check_command ctx cmd in
      c :: check_program ctx' rest
  | [] -> []
