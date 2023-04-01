open Stdlib
open Ast
open Typing
module PTree = Parsetree
module TTree = Templatetree

let check_ref_only_on_var (expr : TTree.t) (ty : Ty_template.t) =
  match (expr.body, ty.body) with
  | Var _, RBase (_, Some _) -> true
  | _, RBase (_, Some _) -> false
  | _ -> true

let check_inline_var_names_match (expr : TTree.t) (ty : Ty_template.t) =
  match (expr.body, ty.body) with
  | Var v, RBase (_, Some ref) -> ref.bound_var = v
  | _ -> true

let check_var_matches_bound_var (var : string) (ty : Ty_surface.t) =
  match ty.body with SBase (_, Some ref) -> ref.bound_var = var | _ -> true

let checks =
  [
    ( "attempted to refine an expression that isn't a variable",
      check_ref_only_on_var );
    ( "bound variable in refinement doesn't match the name of the variable it \
       refines",
      check_inline_var_names_match );
  ]

let check_ref_annotations (expr : TTree.t) (ty : Ty_template.t) =
  let rec iter tests =
    match tests with
    | (msg, test_fn) :: rest -> if test_fn expr ty then iter rest else Error msg
    | [] -> Ok ()
  in
  iter checks
