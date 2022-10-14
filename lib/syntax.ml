(* Abstract syntax. *)

open Printf

(* Variable names *)
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

(* Expressions *)
type expr =
  | Var of ident
  | Int of int
  | Bool of bool
  | Binop of Op.t * expr * expr
  | If of expr * expr * expr
  | LetIn of ident * expr * expr
  | Fun of ident * expr
  | Apply of expr * expr

let rec to_string = function
  | Var v -> v
  | Int i -> Int.to_string i
  | Bool b -> if b then "true" else "false"
  | Binop (op, l, r) ->
    sprintf "%s %s %s" (Op.to_string op) (to_string l) (to_string r)
  | If (cond, if_t, if_f) ->
    sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t) (to_string if_f)
  | LetIn (name, e1, e2) ->
    sprintf "(let %s = %s in %s)" name (to_string e1) (to_string e2)
  | Fun (arg, body) ->
    sprintf "(fun %s -> %s)" arg (to_string body)
  | Apply (e1, e2) ->
    sprintf "%s %s" (to_string e1) (to_string e2)

module Command = struct
  (** 
    A top-level command in the program 
  *)
  type t =
    | Expr of expr
    | LetDef of ident * expr

  let to_string = function
    | Expr e -> to_string e
    | LetDef (name, body) ->
      sprintf "let %s = %s ;;" name (to_string body)
end

module Program = struct
  type t = Command.t list

  let to_string prog = 
      List.map Command.to_string prog
      |> String.concat "\n"
end

type program = Command.t list
