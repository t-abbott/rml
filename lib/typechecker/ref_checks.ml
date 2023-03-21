open Ast 
open Typing

module TTree = Templatetree

let check_ref_only_on_var (expr : TTree.t) (ty: Ty_template.t) = 
  ignore ty; 
  ignore expr; 
  match ty.body with 
  | RBase (_, Some _) -> 
    (match expr.body with 
    | Var _ -> true 
    | _ -> false)
  | _ -> true

(* 
let check_inline_vars_match () = failwith "not implemented"
let check_signature_vars_match () = failwith "not implemented" *)
