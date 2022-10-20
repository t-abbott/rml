open Printf
open Base

(* TODO: namespace parsing under rml
   is that possible?n Just rename to parser   
*)
open Parser
open Parser.Parsetree

module L = Location

module Env = struct 
  type t = (ident * envval) list
  and envval = 
    | Closure of Parsetree.t * t
    | Value of Parsetree.t
 
  let to_string = function
    | Value v -> Parsetree.to_string v
    | Closure (_, _) -> "[closure]" 

  let empty = []
  
  let find name env = List.Assoc.find env name ~equal:String.equal

  let extend (name: ident) (value: envval) env = (name, value) :: env
end

exception InterpError of string * Location.t

let rec eval (expr: t) env =
  match expr.body with
  | Annotated (e, _) ->
    eval e env
  | Integer _ | Boolean _ ->
    Env.Value (expr)
  | Var v ->
    (match Env.find v env with
    | Some value -> value
    | None ->
      let msg = sprintf "reference to unknown variable '%s'" v 
      in raise (InterpError (msg, expr.loc)))
  | Binop (op, l, r) ->
    eval_binop (op, l, r) env 
  | If (cond, if_t, if_f) ->
    if (eval_bool cond env) then
      eval if_t env
    else
      eval if_f env
  | LetIn (name, boundval, body) ->
    let e = eval boundval env 
    in eval body (Env.extend name e env)
  | Fun _ as f ->
    Closure (L.unlocated f, env)
  | Apply (e1, e2) -> 
    let arg = eval e2 env
    in match (eval e1 env) with
    | Closure ({ body=(Fun (x, body)); _ } , bound_env) ->
      let env' = Env.extend x arg bound_env
      in eval body env'   
    | _ ->
      let msg = "tried to apply a value to something that isn't a function"
      in raise (InterpError (msg, expr.loc)) 

and eval_number expr env = 
  match eval expr env with
  | Value ({ body=Integer i; _ }) -> i
  | _ ->
    let msg = "Expected expression to reduce to a number"
    in raise (InterpError (msg, expr.loc))  
and eval_bool expr env = 
  match eval expr env with
  | Value ({ body=Boolean b; _ }) -> b
  | _ ->
    let msg = "Expected expression to reduce to a boolean"
    in raise (InterpError (msg, expr.loc)) 
and eval_binop (op, l, r) env = 
  (match op with
  | Op.Equal ->
    failwith "equals not implemented"
  | Op.Less ->
    let x, y = (eval_number l env), (eval_number r env)
    in Value (L.unlocated (Boolean (x < y)))
  | Op.Greater ->
    let x, y = (eval_number l env), (eval_number r env)
    in Value (L.unlocated (Boolean (x > y)))
  | Op.Plus ->
    let x, y = (eval_number l env), (eval_number r env)
    in Value (L.unlocated (Integer (x + y)))
  | Op.Minus ->
    let x, y = (eval_number l env), (eval_number r env)
    in Value (L.unlocated (Integer (x - y)))
  | Op.Times ->
    let x, y = (eval_number l env), (eval_number r env)
    in Value (L.unlocated (Integer (x * y)))
  | Op.And ->
    let x, y = (eval_bool l env), (eval_bool r env)
    in Value (L.unlocated (Boolean (x && y)))
  | Op.Or -> 
    let x, y = (eval_bool l env), (eval_bool r env)
    in Value (L.unlocated (Boolean (x || y))))

let eval_cmd (cmd: command) env =
  match cmd.body with
  | Expr e ->
    (eval e env), env
  | LetDef (name, body) ->
    let env' = Env.extend name (eval body env) env
    in (Value (L.unlocated (Integer 0))), env' 

let rec run prog env = 
  match prog with
  | cmd :: [] ->
    let res = fst (eval_cmd cmd env)
    in res 
  | cmd :: rest ->
    let new_env = snd (eval_cmd cmd env)
    in run rest new_env
  | [] ->
    failwith "TODO figure out how to do nothing"
