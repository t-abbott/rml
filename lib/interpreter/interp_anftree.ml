open Base
open Printf
open Ast
open Typing
open Typing.Ty_sig
open Errors
open Utils

let third _ _ z = z

module Make (Ty : TYPE) = struct
  module Id = Ident_core
  module Tree = Anftree.Make (Ty)
  module TEnv = Env.Make (Id) (Tree)

  let rec eval_aexpr (ae : Tree.aexpr) env =
    match ae.body with
    | Tree.AInteger _ | Tree.ABoolean _ -> TEnv.Value (Tree.t_of_aexpr ae)
    | Tree.AVar v -> (
        match v with
        | BuiltinSym _ -> TEnv.Value (Tree.t_of_aexpr ae)
        | _ -> (
            match TEnv.find v env with
            | Some value -> value
            | None ->
                let msg =
                  sprintf "reference to unknown variable %s" (Id.to_string v)
                in
                raise (InterpError (msg, ae.loc))))
    | Tree.ALambda _ ->
        let fn = Tree.t_of_aexpr ae in
        TEnv.Closure (fn, env)

  and eval_cexpr (ce : Tree.cexpr) env =
    match ce.body with
    | Tree.CIf (cond, if_t, if_f) ->
        if eval_bool (Tree.t_of_aexpr cond) env then eval_aexpr if_t env
        else eval_aexpr if_f env
    | Tree.CApply (fn, arg) -> (
        (* evalaute [fn] and [arg] down to redexes *)
        let arg' = eval_aexpr arg env in

        (* if fn a builtin var then produce an internal
           if fn an interal then evaluate it with arg
           otherwise evaluate the function *)
        match eval_closure (Tree.t_of_aexpr fn) env with
        | `BuiltinVar v ->
            let op = Op.Binop.of_string v in

            (* create a function that applies the operator to two inputs *)
            let op_fn (r : Tree.t_body Tree.astnode) =
              let op_ty = Ty.ty_of_op op in
              let res_ty =
                match Ty.apply_types op_ty [ arg.ty; r.ty ] with
                | Some ty -> ty
                | None ->
                    unreachable
                      ~reason:
                        "typechecking should have ensured app is over valid \
                         types"
                      ~loc:r.loc
              in
              eval_binop (op, Tree.t_of_aexpr arg, r) env res_ty ce.loc
            in

            TEnv.Internal (op_fn, env)
        | `Internal (fn, _) -> fn (Tree.t_of_aexpr arg)
        | `Closure (param, fn_body, closed_env) ->
            (* extend the env of [fn] to contain [arg] *)
            let new_env = TEnv.extend param arg' closed_env in

            (* evaluate the body of [fn] *)
            eval fn_body new_env)
    | Tree.CAexpr ae -> eval_aexpr ae env

  and eval_const (expr : Tree.t) (env : TEnv.t) =
    let res = eval expr env in
    match res with
    | TEnv.Value v -> (
        match v.body with
        | Tree.CExpr e -> (
            match e.body with
            | Tree.CAexpr ae -> (
                match ae.body with
                | Tree.ABoolean b -> `Bool b
                | Tree.AInteger n -> `Num n
                | Tree.AVar (Id.BuiltinSym v) -> `BuiltinVar v
                | _ -> `Fail)
            | _ -> `Fail)
        | Tree.Let (_, _, _) -> `Fail)
    | TEnv.Closure (defn, closed_env) -> (
        match defn.body with
        | Tree.CExpr e -> (
            match e.body with
            | Tree.CAexpr ce -> (
                match ce.body with
                | Tree.ALambda (params, body) ->
                    `Closure (params, body, closed_env)
                | _ -> `Fail)
            | _ -> `Fail)
        | _ -> `Fail)
    | TEnv.Internal (fn, env) -> `Internal (fn, env)

  and eval_bool expr env =
    match eval_const expr env with
    | `Bool b -> b
    | _ ->
        let msg = "expected expression to reduce to a bool" in
        unreachable ~reason:msg ~loc:expr.loc

  and eval_number expr env =
    match eval_const expr env with
    | `Num n -> n
    | _ ->
        let msg = "expected expression to reduce to a number" in
        unreachable ~reason:msg ~loc:expr.loc

  and eval_closure expr env =
    let res = eval_const expr env in
    match res with
    | `Closure c -> `Closure c
    | `BuiltinVar b -> `BuiltinVar b
    | `Internal fn -> `Internal fn
    | _ ->
        let msg = "expected expression to reduce to a function closure" in
        unreachable ~reason:msg ~loc:expr.loc

  and eval_binop (op, l, r) env (ty : Ty.t) loc =
    match op with
    | Op.Binop.Equal ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_bool (x = y) ty loc in
        TEnv.Value body
    | Op.Binop.Less ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_bool (x < y) ty loc in
        TEnv.Value body
    | Op.Binop.Greater ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_bool (x > y) ty loc in
        TEnv.Value body
    | Op.Binop.Plus ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_number (x + y) ty loc in
        TEnv.Value body
    | Op.Binop.Minus ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_number (x - y) ty loc in
        TEnv.Value body
    | Op.Binop.Times ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_number (x * y) ty loc in
        TEnv.Value body
    | Op.Binop.Div ->
        let x, y = (eval_number l env, eval_number r env) in
        if y = 0 then
          let msg = "attempted to divide by 0" in
          raise (InterpError (msg, loc))
        else
          let body = Tree.t_of_number (x / y) ty loc in
          TEnv.Value body
    | Op.Binop.Mod ->
        let x, y = (eval_number l env, eval_number r env) in
        let body = Tree.t_of_number (x % y) ty loc in
        TEnv.Value body
    | Op.Binop.And ->
        let p, q = (eval_bool l env, eval_bool r env) in
        let body = Tree.t_of_bool (p && q) ty loc in
        TEnv.Value body
    | Op.Binop.Or ->
        let p, q = (eval_bool l env, eval_bool r env) in
        let body = Tree.t_of_bool (p || q) ty loc in
        TEnv.Value body

  and eval (expr : Tree.t) (env : TEnv.t) : TEnv.envval =
    match expr.body with
    | Tree.Let (name, value, rest) ->
        let res = eval_cexpr value env in
        let env' = TEnv.extend name res env in
        eval rest env'
    | Tree.CExpr e -> eval_cexpr e env

  let eval_cmd (expr : Tree.command) (env : TEnv.t) =
    match expr with
    | Tree.LetDef (name, defn) ->
        let var = Tree.var name defn.ty defn.loc in
        let value = eval defn env in
        let new_env = TEnv.extend name value env in
        (TEnv.Value var, new_env)
    | Tree.Expr e -> (eval e env, env)

  let rec run (prog : Tree.program) (env : TEnv.t) =
    match prog with
    | cmd :: [] -> fst (eval_cmd cmd env)
    | cmd :: rest ->
        let new_env = snd (eval_cmd cmd env) in
        run rest new_env
    | [] -> failwith "unreachable"
end
