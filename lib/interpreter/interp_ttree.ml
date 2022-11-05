open Base
open Types
open Ast
open Ast.Typedtree
module L = Utils.Location
module TTEnv = Env.MakeEnv (Typedtree)

let placeholder_value =
  { body = Integer 0; ty = Ty.builtin TInt; loc = L.Nowhere }

let eval _ _ = failwith "not implemented"

let eval_cmd cmd env =
  match cmd with
  | Expr e -> (eval e env, env)
  | LetDef (name, body) ->
      let env' = TTEnv.extend name (eval body env) env in
      ({ placeholder_value with loc = body.loc }, env')

let rec run (prog : command list) (env : TTEnv.t) =
  match prog with
  | cmd :: [] -> fst (eval_cmd cmd env)
  | cmd :: rest ->
      let new_env = snd (eval_cmd cmd env) in
      run rest new_env
  | [] -> failwith "unreachable"