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
  | LetIn (namebinding, boundval, body) ->
      let name =
        match namebinding.body with
        | Var v -> v
        | Annotated ({ body = Var v; _ }, _) -> v
        | _ ->
            let msg =
              sprintf "let must bind a variable, got %s" (to_string namebinding)
            in
            raise (InterpError (msg, expr.loc))
      in
      let e = eval boundval env in
      eval body (PTEnv.extend name e env)
  | Fun _ as f -> Closure (L.unlocated f, env)
  | Apply (f, args) -> (
      (*
        In the parsetree we allow the argument [x] in the term [(fun x -> ...)]
        to be of type [Parsetree.t] since that makes parsing easier. Doing this
        forces us to add a few extra checks when interpreting, though. We do a similar 
        thing with the bound variable in [let] expressions.
      *)
      let extract_param (p : t) : Ident.t =
        match p.body with
        | Var v -> v
        | Annotated ({ body = Var v; _ }, _) -> v
        | _ ->
            let msg =
              sprintf "function argument must be a variable, got '%s'"
                (to_string p)
            in
            raise (InterpError (msg, p.loc))
      in
      let args' = List.map ~f:(fun expr -> eval expr env) args in
      match eval f env with
      | Closure ({ body = Fun (params, body); _ }, bound_env) ->
          let params' = List.map ~f:extract_param params in
          let param_arg_pairs =
            match List.zip params' args' with
            | Ok pairs -> pairs
            | Unequal_lengths ->
                let n_params = List.length params in
                let n_args = List.length args in
                let msg =
                  sprintf
                    "tried to apply %d arguments to a function expecting %d"
                    n_args n_params
                in
                raise (InterpError (msg, expr.loc))
          in
          let new_env =
            List.fold param_arg_pairs ~init:bound_env
              ~f:(fun env (param, arg) -> PTEnv.extend param arg env)
          in
          eval body new_env
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
  | Binop.Div -> 
      let x, y = (eval_number l env, eval_number r env) in
      (match y with 
      | 0 ->
        let msg = "Attempted to divide by 0" in 
        raise (InterpError (msg, r.loc))
      | _ -> Value (L.unlocated (Integer (x / y))))
  | Binop.And ->
      let x, y = (eval_bool l env, eval_bool r env) in
      Value (L.unlocated (Boolean (x && y)))
  | Binop.Or ->
      let x, y = (eval_bool l env, eval_bool r env) in
      Value (L.unlocated (Boolean (x || y)))

let eval_cmd (cmd : command) env =
  match cmd.body with
  | Expr e -> (eval e env, env)
  | LetDef (name, _, body) ->
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
