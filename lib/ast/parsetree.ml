open Printf
open Typing
open Utils

type t = t_body Location.located

and t_body =
  | Annotated of t * Ty_template.t
  | Var of Ident.t
  | Integer of int
  | Boolean of bool
  | Binop of Op.Binop.t * t * t
  | If of t * t * t
  | LetIn of Ident.t * t * t
  | ValIn of Ident.t * Ty_template.t * t
  | Fun of Ident.t list * t
  | Apply of t * t list

let rec to_string (pt : t) =
  match pt.body with
  | Annotated (term, annot) ->
      sprintf "(%s: %s)" (to_string term) (Ty_template.to_string annot)
  | Var v -> v
  | Integer i -> Int.to_string i
  | Boolean b -> if b then "true" else "false"
  | Binop (op, l, r) ->
      sprintf "(%s %s %s)" (to_string l) (Op.Binop.to_string op) (to_string r)
  | If (cond, if_t, if_f) ->
      sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t)
        (to_string if_f)
  | LetIn (name, e1, e2) ->
      sprintf "(let %s = %s in %s)" name (to_string e1) (to_string e2)
  | ValIn (name, ty, rest) ->
      let rest_str = to_string rest in
      let ty_str = Ty_template.to_string ty in
      sprintf "val %s : %s in %s" name ty_str rest_str
  | Fun (params, body) ->
      let param_strs = String.concat " " params in
      sprintf "(fun %s -> %s)" param_strs (to_string body)
  | Apply (f, args) ->
      let arg_strs = String.concat " " (List.map to_string args) in
      sprintf "%s %s" (to_string f) arg_strs

type command = command_body Location.located

and command_body =
  | Expr of t
  | LetDef of Ident.t * t
  | ValDef of Ident.t * Ty_template.t

let command_to_string (cmd : command) =
  match cmd.body with
  | Expr e -> to_string e
  | LetDef (name, body) -> sprintf "let %s = %s ;;" name (to_string body)
  | ValDef (name, ty) ->
      let ty_str = Ty_template.to_string ty in
      sprintf "val %s : %s;;" name ty_str

type program = command list

let program_to_string (prog : program) =
  List.map command_to_string prog |> String.concat "\n"
