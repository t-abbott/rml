open Base
open Typing
open Ast
open Ast.Templatetree
open Errors
module L = Utils.Location
module TTEnv = Env.Make (Templatetree)

let placeholder_ty =
  let (ref : Refinement_core.t) =
    { bound_var = "_"; expr = L.unlocated (Refinement_core.boolean true) }
  in
  Ty_template.builtin (Ty_template.RBase (Base_ty.TInt, Some ref))

let placeholder_value =
  { body = Number 0.; ty = placeholder_ty; loc = L.Nowhere }

let rec eval expr env =
  let loc = expr.loc in
  match expr.body with
  | Var v -> (
      match TTEnv.find v env with
      | Some value -> value
      | _ -> unreachable ~reason:"sema should detect unbound variables" ~loc)
  | Number _ | Boolean _ -> Value expr
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
                  ~loc
          in
          let new_env =
            List.fold param_arg_pairs ~init:closed_env
              ~f:(fun env (param, arg) -> TTEnv.extend param arg env)
          in
          eval body new_env
      | _ ->
          unreachable
            ~reason:"expression should have been checked to be a function" ~loc)

and eval_number expr env =
  match eval expr env with
  | Value { body = Number i; _ } -> i
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

and eval_binop (op, l, r) env =
  let t_bool = Ty_template.unrefined Base_ty.TBool ~source:Source.Builtin in
  let t_int = Ty_template.unrefined Base_ty.TBool ~source:Source.Builtin in

  match op with
  | Op.Binop.Equal ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (Float.( = ) x y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Less ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (Float.( < ) x y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Greater ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Boolean (Float.( > ) x y) in
      let ty = t_bool in
      Value { placeholder_value with body; ty }
  | Op.Binop.Plus ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Number (x +. y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Minus ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Number (x -. y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Times ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Number (x *. y) in
      let ty = t_int in
      Value { placeholder_value with body; ty }
  | Op.Binop.Div ->
      let x, y = (eval_number l env, eval_number r env) in
      if Float.( = ) y 0. then
        let msg = "Attempted to divide by 0" in
        raise (InterpError (msg, r.loc))
      else
        let body = Number (x /. y) in
        let ty = t_int in
        Value { placeholder_value with body; ty }
  | Op.Binop.Mod ->
      let x, y = (eval_number l env, eval_number r env) in
      let body = Number (x %. y) in
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
