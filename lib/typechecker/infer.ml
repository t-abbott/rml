open Printf
open Ast
open Ast.Typedtree
open Typing
open Utils
open Errors
module PTree = Ast.Parsetree
module TTree = Ast.Typedtree

type context = Ty.t Context.t

let t_int = Ty.unrefined Ty_basic.TInt ~source:Builtin
let t_bool = Ty.unrefined Ty_basic.TBool ~source:Builtin

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
      (* Thought: should binops be desugared into normal function application? *)
      let ty_op = Op.Binop.signature op in
      let l', r' = (type_parsetree l ctx, type_parsetree r ctx) in
      let body = Binop (op, l', r') in
      match Ty.apply_types ty_op [ l'.ty; r'.ty ] with
      | Some ty -> { body; ty; loc }
      | None ->
          let msg =
            sprintf "arguments '%s' and '%s' did not match type '%s'"
              (Ty.to_string l'.ty) (Ty.to_string r'.ty) (Ty.to_string ty_op)
          in
          raise (TypeError (msg, loc)))
  | PTree.If (cond, ift, iff) ->
      let typed_cond = type_parsetree cond ctx in
      let typed_ift, typed_iff =
        (type_parsetree ift ctx, type_parsetree iff ctx)
      in
      let body = If (typed_cond, typed_ift, typed_iff) in
      let ty = typed_ift.ty in
      if not (Ty.equal typed_cond.ty t_bool) then
        let t_str = Ty.to_string typed_cond.ty in
        let msg =
          sprintf "expected if statement condition to be a bool, got '%s'" t_str
        in
        raise (TypeError (msg, loc))
      else if not (Ty.equal typed_ift.ty typed_iff.ty) then
        let s1, s2 = (Ty.to_string typed_ift.ty, Ty.to_string typed_iff.ty) in
        let msg =
          sprintf "expected branches to have the same type, got '%s', '%s" s1 s2
        in
        raise (TypeError (msg, loc))
      else { body; ty; loc }
  | PTree.LetIn (name, value, expr) ->
      let typed_value = type_parsetree value ctx in
      let ctx' = Context.extend name typed_value.ty ctx in
      let typed_expr = type_parsetree expr ctx' in
      let ty = typed_expr.ty in
      let body = LetIn (name, typed_value, typed_expr) in
      { body; ty; loc }
  | PTree.Fun (x, expr) ->
      let arg, ty_arg =
        match x.body with
        | Var _ -> failwith "function parameter type inference not implemented"
        | Annotated ({ body = Var v; _ }, t) -> (v, t)
        | _ ->
            let msg = "expected function argument to be a variable" in
            raise (TypeError (msg, loc))
      in
      let ctx' = Context.extend arg ty_arg ctx in
      let typed_expr = type_parsetree expr ctx' in
      let ty = Ty.inferred (Ty.RArrow ([ ty_arg ], typed_expr.ty)) in
      let body = Fun (arg, typed_expr) in
      { body; ty; loc }
  | PTree.Apply (e1, e2) ->
      let typed_e1, typed_e2 = (type_parsetree e1 ctx, type_parsetree e2 ctx) in
      if Ty.is_function typed_e1.ty then
        let body = Apply (typed_e1, typed_e2) in
        let ty =
          match Ty.apply_types typed_e1.ty [ typed_e2.ty ] with
          | Some t -> t
          | None ->
              let t1, t2 = Misc.proj2 Ty.to_string typed_e1.ty typed_e2.ty in
              let msg =
                sprintf
                  "attempted to apply a value of type '%s' to a value of type \
                   '%s'"
                  t1 t2
              in
              raise (TypeError (msg, loc))
        in
        { body; ty; loc }
      else
        let t1, t2 = Misc.proj2 Ty.to_string typed_e1.ty typed_e2.ty in
        let msg =
          sprintf
            "attempted to apply a value of type '%s' to a function of type '%s'"
            t2 t1
        in
        raise (TypeError (msg, loc))
  | PTree.Annotated (expr, ty_stated) ->
      let typed_expr = type_parsetree expr ctx in
      if not (Ty.equal_base typed_expr.ty ty_stated) then
        let stated, inferred =
          Utils.Misc.proj2 Ty.to_string ty_stated typed_expr.ty
        in
        let msg =
          sprintf "provided type '%s' does not match inferred type '%s'" stated
            inferred
        in
        raise (TypeError (msg, loc))
      else { typed_expr with ty = ty_stated }

let type_command (cmd : PTree.command) (ctx : context) : TTree.command * context
    =
  match cmd.body with
  | PTree.Expr ptree ->
      let ttree = type_parsetree ptree ctx in
      (Expr ttree, ctx)
  | PTree.LetDef (name, defn) ->
      let typed_defn = type_parsetree defn ctx in
      let ctx' = Context.extend name typed_defn.ty ctx in
      (LetDef (name, typed_defn), ctx')

let rec type_program prog ctx =
  match prog with
  | cmd :: rest ->
      let typed_cmd, ctx' = type_command cmd ctx in
      typed_cmd :: type_program rest ctx'
  | [] -> []
