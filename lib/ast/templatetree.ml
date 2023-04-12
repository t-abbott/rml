open Printf
open Typing
open Op
open Utils
module PTree = Parsetree

type t = { body : t_body; ty : Ty_template.t; loc : Location.t }

and t_body =
  | Var of Ident.t
  | Number of float
  | Boolean of bool
  | Binop of Binop.t * t * t
  | If of t * t * t
  | LetIn of Ident.t * t * t
  | Fun of Ident.t * t
  | Apply of t * t

let rec to_string { body; ty; _ } =
  let type_str = Ty_template.to_string ty in
  let term_str =
    match body with
    | Var v -> v
    | Number n -> Float.to_string n
    | Boolean b -> Bool.to_string b
    | Binop (op, l, r) ->
        sprintf "%s %s %s" (to_string l) (Binop.to_string op) (to_string r)
    | If (cond, if_t, if_f) ->
        sprintf "if %s then %s else %s" (to_string cond) (to_string if_t)
          (to_string if_f)
    | LetIn (name, value, body) ->
        sprintf "let %s: %s = %s in %s" name type_str (to_string value)
          (to_string body)
    | Fun (param, body) -> sprintf "(%s -> %s)" param (to_string body)
    | Apply (f, arg) -> sprintf "%s %s" (to_string f) (to_string arg)
  in
  term_str

type command = Expr of t | LetDef of Ident.t * t

let command_to_string = function
  | Expr e -> to_string e
  | LetDef (name, body) ->
      sprintf "let %s: %s = %s ;;" name
        (Ty_template.to_string body.ty)
        (to_string body)

type program = command list

let program_to_string p = List.map command_to_string p |> String.concat "\n"
let from body ty loc = { body; ty; loc }
