open Printf
open Rml
open Rml.Types
open Rml.Location
open Op
module PTree = Parser.Parsetree

type ident = string

type t = { body : t_body; ty : Ty.t; loc : Location.t }

and t_body =
  | Var of ident
  | Integer of int
  | Boolean of bool
  | Binop of Binop.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of ident * t
  | Apply of t * t

let rec to_string { body; ty; _ } =
  let ty_str = Ty.to_string ty in
  match body with
  | Var v -> sprintf "(%s: %s)" v ty_str
  | Integer i -> Int.to_string i
  | Boolean b -> Bool.to_string b
  | Binop (op, l, r) ->
      sprintf "(%s %s %s): %s" (Binop.to_string op) (to_string l) (to_string r)
        ty_str
  | If (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s): %s" (to_string cond) (to_string if_t)
        (to_string if_f) ty_str
  | LetIn (name, value, body) ->
      sprintf "(let %s = %s in %s): %s" name (to_string value) (to_string body)
        ty_str
  | Fun (arg, body) -> sprintf "(%s -> %s): %s" arg (to_string body) ty_str
  | Apply (e1, e2) -> sprintf "(%s %s): %s" (to_string e1) (to_string e2) ty_str

exception TypeError of string * Location.t
exception NameError of string * Location.t

(**
  Build an explicity typed [Typedtree.t] from a partially 
  typed [Parsetree.t].

  Performs type checking and inference.

  {1 FIXME}
  - handle annotations 
  - perform inference on untyped arguments

  {1 Notes}
  Should type inference and type checking happen in different passes? Motivating
  examples are in the branches for [PTree.Fun] and [PTree.Apply], which would be
  much cleaner if they only constructed the [TypedTree.t] and passed it on to an
  inference/semantic analysis pass for further inspection. The tricky part is 

*)
let rec from_parsetree (pt : PTree.t) (ctx : Ty.t Context.t) =
  let loc = pt.loc in
  match pt.body with
  | PTree.Integer i ->
      let body = Integer i in
      let ty = inferred TInt in
      { body; ty; loc }
  | PTree.Boolean b ->
      let body = Boolean b in
      let ty = inferred TBool in
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
  | PTree.Binop (ptop, l, r) -> (
      let op = Op.Binop.of_ptreeop ptop in
      let ty_op = Op.Binop.signature op in
      let l', r' = (from_parsetree l ctx, from_parsetree r ctx) in
      let body = Binop (op, l', r') in
      match apply_args ty_op [ l'.ty; r'.ty ] with
      | Some ty -> { body; ty; loc }
      | _ ->
          let msg =
            sprintf "arguments did not match type '%s'" (Ty.to_string ty_op)
          in
          raise (TypeError (msg, loc)))
  | PTree.If (cond, ift, iff) ->
      let typed_cond = from_parsetree cond ctx in
      let typed_ift, typed_iff =
        (from_parsetree ift ctx, from_parsetree iff ctx)
      in
      let body = If (typed_cond, typed_ift, typed_iff) in
      let ty = typed_ift.ty in
      if not (Ty.equal typed_cond.ty (inferred TBool)) then
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
      let typed_value = from_parsetree value ctx in
      let ty = typed_value.ty in
      let ctx' = Context.extend name typed_value.ty ctx in
      let body = LetIn (name, typed_value, from_parsetree expr ctx') in
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
      let typed_expr = from_parsetree expr ctx' in
      let ty = typed_expr.ty in
      let body = Fun (arg, typed_expr) in
      { body; ty; loc }
  | PTree.Apply (e1, e2) ->
      let typed_e1, typed_e2 = (from_parsetree e1 ctx, from_parsetree e2 ctx) in
      if Ty.is_function typed_e1.ty then
        let body = Apply (typed_e1, typed_e2) in
        let ty =
          match apply_args typed_e1.ty [ typed_e2.ty ] with
          | Some t -> t
          | None ->
              let t1, t2 = Utils.proj2 Ty.to_string typed_e1.ty typed_e2.ty in
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
        let msg = "noo" in
        raise (TypeError (msg, loc))
  | PTree.Annotated (expr, ty_stated) ->
      let typed_expr = from_parsetree expr ctx in
      if not (Ty.equal typed_expr.ty ty_stated) then
        let stated, inferred =
          Utils.proj2 Ty.to_string ty_stated typed_expr.ty
        in
        let msg =
          sprintf "provided type '%s' does not match inferred type '%s'" stated
            inferred
        in
        raise (TypeError (msg, loc))
      else typed_expr
