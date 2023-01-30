open Base
open Printf
open Ast
open Ast.Templatetree
open Typing
open Utils
open Errors
module PTree = Ast.Parsetree
module TTree = Ast.Templatetree

type context = Ty_template.t Context.t

let t_int = Ty_template.unrefined Base_ty.TInt ~source:Builtin
let t_bool = Ty_template.unrefined Base_ty.TBool ~source:Builtin

let rec type_parsetree (pt : PTree.t) ctx =
  let loc = pt.loc in
  match pt.body with
  | PTree.Integer i ->
      let body = Integer i in
      let ty = t_int in
      { body; ty; loc }
  | PTree.Boolean b ->
      let body = Boolean b in
      let ty = t_bool in
      { body; ty; loc }
  | PTree.Var v ->
      let ty =
        match Context.find v ctx with
        | Some ty' -> ty'
        | None ->
            (* TODO: handle this in a semantic analysis pass on the parsetree? *)
            let msg = sprintf "reference to unknown variable '%s'" v in
            raise (NameError (msg, loc))
      in
      let body = Var v in
      { body; ty; loc }
  | PTree.Binop (op, l, r) -> (
      let ty_op = Op.Binop.signature op in
      let l', r' = (type_parsetree l ctx, type_parsetree r ctx) in
      let body = Binop (op, l', r') in

      (* jsdkf *)
      match Ty_template.apply_types ty_op [ l'.ty; r'.ty ] with
      | Some ty -> { body; ty; loc }
      | None ->
          let msg =
            sprintf
              "arguments of type '%s' and '%s' did not match expected type '%s'"
              (Ty_template.to_string l'.ty)
              (Ty_template.to_string r'.ty)
              (Ty_template.to_string ty_op)
          in
          raise (TypeError (msg, loc)))
  | PTree.If (cond, ift, iff) ->
      let typed_cond = type_parsetree cond ctx in
      let typed_ift, typed_iff =
        (type_parsetree ift ctx, type_parsetree iff ctx)
      in
      let body = If (typed_cond, typed_ift, typed_iff) in
      let ty = typed_ift.ty in
      if not (Ty_template.equal typed_cond.ty t_bool) then
        let t_str = Ty_template.to_string typed_cond.ty in
        let msg =
          sprintf "expected if statement condition to be a bool, got '%s'" t_str
        in
        raise (TypeError (msg, loc))
      else if not (Ty_template.equal typed_ift.ty typed_iff.ty) then
        let s1, s2 =
          ( Ty_template.to_string typed_ift.ty,
            Ty_template.to_string typed_iff.ty )
        in
        let msg =
          sprintf "expected branches to have the same type, got '%s', '%s" s1 s2
        in
        raise (TypeError (msg, loc))
      else { body; ty; loc }
  | PTree.LetIn (namebinding, value, expr) ->
      let typed_value = type_parsetree value ctx in
      let name =
        match namebinding.body with
        | PTree.Var v -> v
        | PTree.Annotated ({ body = Var v; _ }, ty_stated) ->
            let ty_t_stated = Ty_template.of_surface ty_stated in
            if not (Ty_template.equal_base ty_t_stated typed_value.ty) then
              let msg =
                sprintf "stated type '%s' does not match inferred type '%s'"
                  (Ty_template.to_string ty_t_stated)
                  (Ty_template.to_string typed_value.ty)
              in
              raise (TypeError (msg, namebinding.loc))
            else v
        | _ ->
            let msg =
              sprintf "let statements must bind a variable, got '%s'"
                (PTree.to_string namebinding)
            in
            raise (TypeError (msg, namebinding.loc))
      in
      let ctx' = Context.extend name typed_value.ty ctx in
      let typed_expr = type_parsetree expr ctx' in
      let ty = typed_expr.ty in
      let body = LetIn (name, typed_value, typed_expr) in
      { body; ty; loc }
  | PTree.Fun (xs, expr) ->
      let param_ty_pairs =
        List.map
          ~f:(fun param ->
            match param.body with
            | Var _ ->
                failwith "function parameter type inference not implemented"
            | Annotated ({ body = Var v; _ }, t) -> (v, Ty_template.of_surface t)
            | _ ->
                let msg = "expected function argument to be a variable" in
                raise (TypeError (msg, loc)))
          xs
      in
      let ctx' =
        List.fold ~init:ctx
          ~f:(fun ctx (param, ty) -> Context.extend param ty ctx)
          param_ty_pairs
      in
      let typed_expr = type_parsetree expr ctx' in
      let params = List.map ~f:fst param_ty_pairs in
      let param_tys = List.map ~f:snd param_ty_pairs in
      let ty =
        Ty_template.inferred (Ty_template.RArrow (param_tys, typed_expr.ty))
      in
      let body = Fun (params, typed_expr) in
      { body; ty; loc }
  | PTree.LetFun (name, params, body, rest) ->
      (* desugar named functions to lambdas and then type *)
      let fname = Location.locate loc (PTree.Var name) in
      let lambda = Location.locate loc (PTree.Fun (params, body)) in
      let bound_fn = Location.locate loc (PTree.LetIn (fname, lambda, rest)) in
      type_parsetree bound_fn ctx
  | PTree.Apply (e1, e2s) ->
      let typed_e1, typed_e2s =
        (type_parsetree e1 ctx, List.map ~f:(fun e -> type_parsetree e ctx) e2s)
      in
      if Ty_template.is_function typed_e1.ty then
        let body = Apply (typed_e1, typed_e2s) in
        let ty =
          let arg_tys = List.map ~f:(fun e -> e.ty) typed_e2s in

          (* test we're applying the right number of arguments *)
          let n_params = Ty_template.arity typed_e1.ty in
          let n_args = List.length e2s in
          if n_args <> n_params then
            let msg =
              sprintf
                "attemped to apply %d arguments to a function of %d parameters"
                n_args n_params
            in
            raise (TypeError (msg, loc))
          else
            (* test argument types match *)
            match Ty_template.apply_types typed_e1.ty arg_tys with
            | Some t -> t
            | None ->
                let f_ty_str = Ty_template.to_string typed_e1.ty in
                let arg_ty_strs =
                  List.map ~f:(fun e -> Ty_template.to_string e.ty) typed_e2s
                in
                let msg =
                  sprintf
                    "attempted to apply arguments of type '%s' to a function \
                     of type '%s'"
                    f_ty_str
                    (String.concat ~sep:" " arg_ty_strs)
                in
                raise (TypeError (msg, loc))
        in
        { body; ty; loc }
      else
        let s1 = Ty_template.to_string typed_e1.ty in
        let msg =
          sprintf "attempted to apply to a non-function value of type '%s'" s1
        in
        raise (TypeError (msg, loc))
  | PTree.Annotated (expr, ty_stated) ->
      let ty_t_stated = Ty_template.of_surface ty_stated in
      let typed_expr = type_parsetree expr ctx in
      if not (Ty_template.equal_base typed_expr.ty ty_t_stated) then
        let stated, inferred =
          Utils.Misc.proj2 Ty_template.to_string ty_t_stated typed_expr.ty
        in
        let msg =
          sprintf "provided type '%s' does not match inferred type '%s'" stated
            inferred
        in
        raise (TypeError (msg, loc))
      else { typed_expr with ty = ty_t_stated }

let type_command (cmd : PTree.command) (ctx : context) :
    TTree.command option * context =
  match cmd.body with
  | PTree.Expr ptree ->
      let ttree = type_parsetree ptree ctx in
      (Some (Expr ttree), ctx)
  | PTree.LetDef (name, ty_annotated, defn) ->
      (* convert any annotated [Ty_surface.t] to a [Ty_template.t] *)
      let ty_t_annotated =
        match ty_annotated with
        | Some t -> Some (Ty_template.of_surface t)
        | None -> None
      in

      (* get the type of [name] as declared in a [val] statement (if any) *)
      let ty_t_valdef = Context.find name ctx in

      (* test that the annotated type matched the val type (if both are present) *)
      let ty_t_stated =
        match (ty_t_annotated, ty_t_valdef) with
        | Some ty_annot, Some ty_val ->
            if not (Ty_template.equal_base ty_annot ty_val) then
              let msg =
                sprintf
                  "type mistmatch - declared '%s' but was annotated with '%s'"
                  (Ty_template.to_string ty_val)
                  (Ty_template.to_string ty_annot)
              in
              raise (TypeError (msg, cmd.loc))
            else Some ty_annot
        | Some t, None | None, Some t -> Some t
        | None, None -> None
      in

      (* infer the type of [defn] *)
      let ty_t_inferred = type_parsetree defn ctx in

      (* test the stated type matches the inferred type *)
      let typed_defn =
        match (ty_t_inferred, ty_t_stated) with
        | tdefn, None -> tdefn
        | tdefn, Some ty when Ty_template.equal_base tdefn.ty ty ->
            (* prefer the annotated type as it may contain refinement annotations *)
            { tdefn with ty }
        | tdefn, Some ty_bad ->
            let msg =
              sprintf "stated type '%s' doesn't match expected type '%s'"
                (Ty_template.to_string ty_bad)
                (Ty_template.to_string tdefn.ty)
            in
            raise (TypeError (msg, cmd.loc))
      in
      (* otherwise the expression is well-typed and we can add it to the environment *)
      let ctx' = Context.extend name typed_defn.ty ctx in
      (Some (LetDef (name, typed_defn)), ctx')
  | PTree.ValDef (name, ty) ->
      let ty_t = Ty_template.of_surface ty in
      let ctx' = Context.extend name ty_t ctx in
      (None, ctx')

let rec type_program prog ctx =
  match prog with
  | cmd :: rest -> (
      match type_command cmd ctx with
      | Some typed_cmd, ctx' -> typed_cmd :: type_program rest ctx'
      | None, ctx' -> type_program rest ctx')
  | [] -> []
