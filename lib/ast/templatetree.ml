open Printf
open Typing
open Utils
module PTree = Parsetree
module Id = Ident_core

type t = { body : t_body; ty : Ty_template.t; loc : Location.t }

and t_body =
  | Var of Id.t
  | Number of float
  | Boolean of bool
  | If of t * t * t
  | LetIn of Id.t * t * t
  | Fun of Id.t * t
  | Apply of t * t

let rec to_string { body; ty; _ } =
  let type_str = Ty_template.to_string ty in
  let term_str =
    match body with
    | Var v -> Id.to_string v
    | Number n -> Float.to_string n
    | Boolean b -> Bool.to_string b
    | If (cond, if_t, if_f) ->
        sprintf "if %s then %s else %s" (to_string cond) (to_string if_t)
          (to_string if_f)
    | LetIn (name_id, value, body) ->
        sprintf "let %s: %s = %s in %s" (Id.to_string name_id) type_str
          (to_string value) (to_string body)
    | Fun (param_id, body) ->
        sprintf "(%s -> %s)" (Id.to_string param_id) (to_string body)
    | Apply (f, arg) -> sprintf "%s %s" (to_string f) (to_string arg)
  in
  term_str

type command = Expr of t | LetDef of Id.t * t

let command_to_string = function
  | Expr e -> to_string e
  | LetDef (name_id, body) ->
      sprintf "let %s: %s = %s ;;" (Id.to_string name_id)
        (Ty_template.to_string body.ty)
        (to_string body)

type program = command list

let program_to_string p = List.map command_to_string p |> String.concat "\n"
let from body ty loc = { body; ty; loc }
