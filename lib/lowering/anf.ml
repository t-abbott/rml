open Ast
open Ast.Lineartree
open Typing
open Utils
module TTree = Templatetree

type hole = aexpr -> t
type t_hole = t -> t

let id x = x

let initial_hole (ty : Ty_template.t) (loc : Location.t) (ae : aexpr) : t =
  let ca_node = node_of (CAexpr ae) ty loc in
  let ce_node = node_of (CExpr ca_node) ty loc in
  ce_node

let rec anf_inner (expr : TTree.t) (hole : hole) : t =
  let ty, loc = (expr.ty, expr.loc) in
  match expr.body with
  | TTree.Var v -> node_of (AVar (Ident_core.var v)) ty loc |> hole
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
  | TTree.LetIn (var_name, value, rest) ->
      let new_name = Ident_core.var var_name in

      anf_inner value (fun v_ae ->
          let value_node = node_of (CAexpr v_ae) ty loc in
          node_of (Let (new_name, value_node, anf_inner rest hole)) ty loc)
  | TTree.Fun (params, body) ->
      let new_params = List.map Ident_core.var params in
      let new_body = anf body in
      node_of (ALambda (new_params, new_body)) ty loc |> hole
  | TTree.Apply (fn, args) ->
      let res_name = Ident_core.fresh () in
      let res_name_node = node_of (AVar res_name) ty loc in

      (* Since a function can take multiple arguments we need to get
         an aexpr representing each parameter separately and bind the
         normalisation of each argument in a sequence of [Let] expressions.
         [fold_right] applies [lower_args] to the final element of [args]
         first, so the resulting intermediate variable names [gen_names]
         should be in the correct order.
      *)
      anf_inner fn (fun fn_ae ->
          (* The initial t-hole we pass to [fold_right] is forwarded through each
             argument let-binding and simply returns it's argument since we'll
             give it the final let-binding containing the result of applying [fn] to
             the generated immediate values (whose names aren't known until we fold
             lower_args over args).

             This is why a different hole type is necessary - if we tried to use a [hole : aexpr -> t]
             we'd need to pass a custom [hole] up-front where the names of the resulting
             immediate argument names are known, which is impossible.
          *)
          let res_hole, generated_names =
            List.fold_right lower_args args (id, [])
          in

          (* build the final let-binding containing the result of [fn args] *)
          let app_node = node_of (CApply (fn_ae, generated_names)) ty loc in
          let res_node =
            node_of (Let (res_name, app_node, hole res_name_node)) ty loc
          in
          res_hole res_node)

and lower_args arg_expr (next_t_hole, gen_names) : t_hole * aexpr list =
  let ty, loc = (arg_expr.ty, arg_expr.loc) in
  let name = Ident_core.fresh ~prefix:"arg" () in
  let name_node = node_of (AVar name) ty loc in

  let new_t_hole other_let_exp =
    anf_inner arg_expr (fun arg_ae ->
        let value_node = node_of (CAexpr arg_ae) ty loc in
        node_of (Let (name, value_node, next_t_hole other_let_exp)) ty loc)
  in
  (new_t_hole, name_node :: gen_names)

and anf (expr : TTree.t) =
  let ty, loc = (expr.ty, expr.loc) in
  let hole = initial_hole ty loc in
  anf_inner expr hole

let anf_command = function
  | TTree.Expr e -> Expr (anf e)
  | TTree.LetDef (name, body) ->
      let new_name = Ident_core.var name in
      LetDef (new_name, anf body)

let anf_program = List.map anf_command
