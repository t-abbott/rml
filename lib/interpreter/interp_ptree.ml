open Printf
open Base
open Ast
open Ast.Op
open Ast.Parsetree
module L = Utils.Location
open Errors
module PTEnv = Env.MakeEnv (Parsetree)

let rec eval (expr : t) env =
  match expr.body with
  | Annotated (e, _) -> eval e env
  | Integer _ | Boolean _ -> PTEnv.Value expr
  | Var v -> (
      match PTEnv.find v env with
      | Some value -> value
      | None ->
          let msg = sprintf "reference to unknown variable '%s'" v in
          raise (InterpError (msg, expr.loc)))
  | Binop (op, l, r) -> eval_binop (op, l, r) env
  | If (cond, if_t, if_f) ->
      if eval_bool cond env then eval if_t env else eval if_f env
  | LetIn (name, boundval, body) ->
      let e = eval boundval env in
      eval body (PTEnv.extend name e env)
  | Fun _ as f -> Closure (L.unlocated f, env)
  | Apply (e1, e2) -> (
      (*
         In the parsetree we allow the argument [x] in the term [(fun x -> ...)]
         to be of type [Parsetree.t] since that makes parsing easier. It forces to
         add extra checks when interpreting, however.
      *)
      let arg = eval e2 env in
      match eval e1 env with
      | Closure ({ body = Fun (x, body); _ }, bound_env) ->
          let x' =
            match x.body with
            | Var v -> v
            | Annotated ({ body = Var v; _ }, _) -> v
            | _ ->
                let msg =
                  sprintf "function argument must be a variable, got '%s'"
                    (to_string x)
                in
                raise (InterpError (msg, x.loc))
          in
          let env' = PTEnv.extend x' arg bound_env in
          eval body env'
      | _ ->
          let msg =
            "tried to apply a value to something that isn't a function"
          in
          raise (InterpError (msg, expr.loc)))

and eval_number expr env =
  match eval expr env with
  | Value { body = Integer i; _ } -> i
  | _ ->
      let msg = "Expected expression to reduce to a number" in
      raise (InterpError (msg, expr.loc))

and eval_bool expr env =
  match eval expr env with
  | Value { body = Boolean b; _ } -> b
  | _ ->
      let msg = "Expected expression to reduce to a boolean" in
      raise (InterpError (msg, expr.loc))

and eval_binop (op, l, r) env =
  match op with
  | Binop.Equal ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Boolean (x = y)))
  | Binop.Less ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Boolean (x < y)))
  | Binop.Greater ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Boolean (x > y)))
  | Binop.Plus ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Integer (x + y)))
  | Binop.Minus ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Integer (x - y)))
  | Binop.Times ->
      let x, y = (eval_number l env, eval_number r env) in
      Value (L.unlocated (Integer (x * y)))
  | Binop.And ->
      let x, y = (eval_bool l env, eval_bool r env) in
      Value (L.unlocated (Boolean (x && y)))
  | Binop.Or ->
      let x, y = (eval_bool l env, eval_bool r env) in
      Value (L.unlocated (Boolean (x || y)))

let eval_cmd (cmd : command) env =
  match cmd.body with
  | Expr e -> (eval e env, env)
  | LetDef (name, body) ->
      let env' = PTEnv.extend name (eval body env) env in
      (Value (L.unlocated (Integer 0)), env')

let rec run prog env =
  match prog with
  | cmd :: [] ->
      let res = fst (eval_cmd cmd env) in
      res
  | cmd :: rest ->
      let new_env = snd (eval_cmd cmd env) in
      run rest new_env
  | [] -> failwith "TODO figure out how to do nothing"
