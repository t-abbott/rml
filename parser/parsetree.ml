open Printf

open Rml
open Rml.Types

type ident = string

module Op = struct
  type t =
    | Equal 
    | Less
    | Greater
    | Plus 
    | Minus
    | Times 
    | And
    | Or

    let to_string = function
      | Equal -> "="
      | Less -> "<"
      | Greater -> ">"
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | And -> "&&"
      | Or -> "||"
end

type ty = Empty

type t = t_body Location.located
and t_body =
  | Annotated of t * Ty.t
  | Var of ident
  | Integer of int
  | Boolean of bool
  | Binop of Op.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of t * t
  | Apply of t * t

let rec to_string (pt: t) =
  match pt.body with
  | Annotated (term, annot) ->
    sprintf "(%s: %s)" (to_string term) (Ty.to_string annot) 
  | Var v -> v
  | Integer i -> Int.to_string i
  | Boolean b -> if b then "true" else "false"
  | Binop (op, l, r) ->
    sprintf "(%s %s %s)" (to_string l) (Op.to_string op) (to_string r)
  | If (cond, if_t, if_f) ->
    sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t) (to_string if_f)
  | LetIn (name, e1, e2) ->
    sprintf "(let %s = %s in %s)" name (to_string e1) (to_string e2)
  | Fun (arg, body) ->
    sprintf "(fun %s -> %s)" (to_string arg) (to_string body)
  | Apply (e1, e2) ->
    sprintf "%s %s" (to_string e1) (to_string e2)
    
type command = command_body Location.located
and command_body =
  | Expr of t
  | LetDef of ident * t

let command_to_string (cmd: command) =
  match cmd.body with
  | Expr e -> to_string e
  | LetDef (name, body) ->
    sprintf "let %s = %s ;;" name (to_string body)

type program = command list

let program_to_string (prog: program) = 
  List.map command_to_string prog
  |> String.concat "\n"
