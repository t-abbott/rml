open Printf
open Typing
open Op
open Utils
module PTree = Parsetree

type t = { body : t_body; ty : Ty.t; loc : Location.t }

and t_body =
  | Var of Ident.t
  | Integer of int
  | Boolean of bool
  | Binop of Binop.t * t * t
  | If of t * t * t
  | LetIn of Ident.t * t * t
  | Fun of Ident.t * t
  | Apply of t * t

let rec to_string { body; ty; _ } =
  let type_str = Ty.to_string ty in
  let term_str =
    match body with
    | Var v -> v
    | Integer i -> Int.to_string i
    | Boolean b -> Bool.to_string b
    | Binop (op, l, r) ->
        sprintf "%s %s %s" (to_string l) (Binop.to_string op) (to_string r)
    | If (cond, if_t, if_f) ->
        sprintf "if %s then %s else %s" (to_string cond) (to_string if_t)
          (to_string if_f)
    | LetIn (name, value, body) ->
        sprintf "let %s = %s in %s" name (to_string value) (to_string body)
    | Fun (arg, body) -> sprintf "%s -> %s" arg (to_string body)
    | Apply (e1, e2) -> sprintf "%s %s" (to_string e1) (to_string e2)
  in
  sprintf "%s: %s" term_str type_str

type command = Expr of t | LetDef of Ident.t * t

let command_to_string = function
  | Expr e -> to_string e
  | LetDef (name, body) ->
      sprintf "let %s: %s = %s ;;" name (Ty.to_string body.ty) (to_string body)

type program = command list

let program_to_string p = List.map command_to_string p |> String.concat "\n"
