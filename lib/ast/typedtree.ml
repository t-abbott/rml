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
  let ty_str = Ty.to_string ty in
  match body with
  | Var v -> sprintf "(%s: %s)" v ty_str
  | Integer i -> Int.to_string i
  | Boolean b -> Bool.to_string b
  | Binop (op, l, r) ->
      sprintf "(%s %s %s): %s" (Binop.to_string op) (to_string l) (to_string r)
        ty_str
  | If (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s): %s" (to_string cond) (to_string if_t)
        (to_string if_f) ty_str
  | LetIn (name, value, body) ->
      sprintf "(let %s = %s in %s): %s" name (to_string value) (to_string body)
        ty_str
  | Fun (arg, body) -> sprintf "(%s -> %s): %s" arg (to_string body) ty_str
  | Apply (e1, e2) -> sprintf "(%s %s): %s" (to_string e1) (to_string e2) ty_str

type command = Expr of t | LetDef of Ident.t * t

let command_to_string = function
  | Expr e -> to_string e
  | LetDef (name, body) ->
      sprintf "let %s: %s = %s ;;" name (Ty.to_string body.ty) (to_string body)

type program = command list

let program_to_string p = List.map command_to_string p |> String.concat "\n"
