open Base
open Typing
open Ast
open Ast.Templatetree
open Errors
open Utils
module L = Location
module TTEnv = Env.Make (Utils.Ident_core) (Templatetree)

let placeholder_ty = Ty_template.t_bool "v"

let placeholder_value =
  { body = Integer 0; ty = placeholder_ty; loc = L.Nowhere }

let rec eval expr env =
  let loc = expr.loc in
  match expr.body with
  | Var v -> (
      match v with
      | BuiltinSym _ -> TTEnv.Value expr
      | _ -> (
          match TTEnv.find v env with
          | Some value -> value
          | _ -> unreachable ~reason:"sema should detect unbound variables" ~loc
          ))
  | Integer _ | Boolean _ -> Value expr
  | If (cond, if_t, if_f) ->
      if eval_bool cond env then eval if_t env else eval if_f env
  | LetIn (name, value, body) ->
      let env' = TTEnv.extend name (eval value env) env in
      eval body env'
  | Fun _ as body -> Closure ({ placeholder_value with body }, env)
  | Apply (f, arg) -> (
      let arg' = eval arg env in
      match eval f env with
      | Value { body = Var v; _ } -> (
          (* check if we're trying to call a builtin function *)
          match v with
          | Utils.Ident_core.BuiltinSym s ->
              let op = Op.Binop.of_string s in
              let eval_fn r = eval_binop (op, arg, r) env in
              Internal (eval_fn, env)
          | _ -> unreachable ~reason:"" ~loc)
      | Internal (fn, _) ->
          (* also a builtin function *)
          fn arg
      | Closure ({ body = Fun (param, body); _ }, closed_env) ->
          let new_env = TTEnv.extend param arg' closed_env in
          eval body new_env
      | _ ->
          unreachable
            ~reason:"expression should have been checked to be a function" ~loc)

and eval_number expr env =
  match eval expr env with
  | Value { body = Integer i; _ } -> i
  | _ ->
      unreachable
        ~reason:"expression should have been checked to reduce to an integer"
        ~loc:expr.loc

and eval_bool expr env =
  match eval expr env with
  | Value { body = Boolean b; _ } -> b
  | _ ->
      unreachable
        ~reason:"expression should have been checked to reduce to a boolean"
        ~loc:expr.loc

and eval_binop (op, l, r) (env : TTEnv.t) =
  let open TTEnv in
  let t_bool = Ty_template.t_bool "v" in
  let t_int = Ty_template.t_num "v" in

  match op with
  | Op.Binop.Equal ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (x = y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Less ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (x < y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Greater ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (x > y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Plus ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Integer (x + y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Minus ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Integer (x - y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Times ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Integer (x * y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Div ->
      let x, y = (eval_number l env, eval_number r env) in
      if y = 0 then
        let msg = "Attempted to divide by 0" in
        raise (InterpError (msg, r.loc))
      else
        let body = Integer (x / y) in
        let ty = t_int in
        Value { placeholder_value with body; ty }
  | Op.Binop.Mod ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Integer (x % y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.And ->
      let x, y = (eval_bool l env, eval_bool r env) in
      let body = Boolean (x && y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Or ->
      let x, y = (eval_bool l env, eval_bool r env) in
      let body = Boolean (x || y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }

let eval_cmd cmd env =
  match cmd with
  | Expr e -> (eval e env, env)
  | LetDef (name, body) ->
      let env' = TTEnv.extend name (eval body env) env in
      (Value { placeholder_value with loc = body.loc }, env')

let rec run (prog : command list) (env : TTEnv.t) =
  match prog with
  | cmd :: [] -> fst (eval_cmd cmd env)
  | cmd :: rest ->
      let new_env = snd (eval_cmd cmd env) in
      run rest new_env
  | [] -> failwith "unreachable"
