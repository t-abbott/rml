open Ast
open Ast.Lineartree
open Typing
open Utils
module TTree = Templatetree

type hole = aexpr -> t
(*
   let translate_binop op l r = failwith "not implemented" *)

let initial_hole (ty : Ty_template.t) (loc : Location.t) (ae : aexpr) : t =
  let ca_node = node_of (CAexpr ae) ty loc in
  let ce_node = node_of (CExpr ca_node) ty loc in
  ce_node

let rec anf_inner (expr : TTree.t) (hole : hole) : t =
  let ty, loc = (expr.ty, expr.loc) in
  match expr.body with
  | TTree.Var v -> node_of (AVar v) ty loc |> hole
  | TTree.Number n -> node_of (ANumber n) ty loc |> hole
  | TTree.Boolean b -> node_of (ABoolean b) ty loc |> hole
  | TTree.Binop (op, l, r) ->
      (* assign the result of [op l r] to a variable and return the variable *)
      let res_name = Ident_core.fresh () in
      let res_name_node = node_of (AVar res_name) ty loc in

      (* convert the left and right branches *)
      anf_inner l (fun l_ae ->
          anf_inner r (fun r_ae ->
              let res = node_of (CBinop (op, l_ae, r_ae)) ty loc in
              node_of (Let (res_name, res, hole res_name_node)) ty loc))
  | TTree.If (cond, if_t, if_f) ->
      let res_name = Ident_core.fresh () in
      let res_name_node = node_of (AVar res_name) ty loc in

      anf_inner cond (fun c_ae ->
          anf_inner if_t (fun t_ae ->
              anf_inner if_f (fun f_ae ->
                  let res = node_of (CIf (c_ae, t_ae, f_ae)) ty loc in
                  node_of (Let (res_name, res, hole res_name_node)) ty loc)))
  | TTree.LetIn (name, value, rest) ->
      anf_inner value (fun v_ae ->
          let value_node = node_of (CAexpr v_ae) ty loc in
          node_of (Let (name, value_node, anf_inner rest hole)) ty loc)
  | TTree.Fun (param, body) ->
      let new_body = anf body in
      node_of (ALambda (param, new_body)) ty loc |> hole
  | TTree.Apply (fn, arg) ->
      let res_name = Ident_core.fresh () in
      let res_name_node = node_of (AVar res_name) ty loc in

      anf_inner fn (fun fn_ae ->
          anf_inner arg (fun arg_ae ->
              let app_node = node_of (CApply (fn_ae, arg_ae)) ty loc in
              node_of (Let (res_name, app_node, hole res_name_node)) ty loc))

and anf (expr : TTree.t) =
  let ty, loc = (expr.ty, expr.loc) in
  let hole = initial_hole ty loc in
  anf_inner expr hole

let anf_command = function
  | TTree.Expr e -> Expr (anf e)
  | TTree.LetDef (name, body) -> LetDef (name, anf body)

let anf_program = List.map anf_command
