open Base
open Typing
open Ast
open Ast.Templatetree
module L = Utils.Location
module TTEnv = Env.MakeEnv (Templatetree)

let placeholder_value =
  {
    body = Integer 0;
    ty =
      Ty_template.builtin
        (Ty_template.RBase
           (Base_ty.TInt, Some (L.unlocated (Refinement.boolean true))));
    loc = L.Nowhere;
  }

let unreachable ~reason =
  let message = "tried to execute branch unreachable because " ^ reason in
  failwith message

let rec eval expr env =
  match expr.body with
  | Var v -> (
      match TTEnv.find v env with
      | Some value -> value
      | _ -> unreachable ~reason:"sema should detect unbound variables")
  | Integer _ | Boolean _ -> Value expr
  | Binop (op, l, r) -> eval_binop (op, l, r) env
  | If (cond, if_t, if_f) ->
      if eval_bool cond env then eval if_t env else eval if_f env
  | LetIn (name, value, body) ->
      let env' = TTEnv.extend name (eval value env) env in
      eval body env'
  | Fun _ as body -> Closure ({ placeholder_value with body }, env)
  | Apply (f, args) -> (
      let args' = List.map ~f:(fun e -> eval e env) args in
      match eval f env with
      | Closure ({ body = Fun (params, body); _ }, closed_env) ->
          let param_arg_pairs =
            match List.zip params args' with
            | Ok pairs -> pairs
            | Unequal_lengths ->
                unreachable
                  ~reason:
                    "type checking should have detected mismatched arg lengths"
          in
          let new_env =
            List.fold param_arg_pairs ~init:closed_env
              ~f:(fun env (param, arg) -> TTEnv.extend param arg env)
          in
          eval body new_env
      | _ ->
          unreachable
            ~reason:"expression should have been checked to be a function")

and eval_number expr env =
  match eval expr env with
  | Value { body = Integer i; _ } -> i
  | _ ->
      unreachable
        ~reason:"expression should have been checked to reduce to an integer"

and eval_bool expr env =
  match eval expr env with
  | Value { body = Boolean b; _ } -> b
  | _ ->
      unreachable
        ~reason:"expression should have been checked to reduce to a boolean"

and eval_binop (op, l, r) env =
  let t_bool = Ty_template.unrefined Base_ty.TBool ~source:Source.Builtin in
  let t_int = Ty_template.unrefined Base_ty.TBool ~source:Source.Builtin in

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
