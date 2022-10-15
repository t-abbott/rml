open Printf

(* Variable names *)
type ident = string

type ty =
  | TBool
  | TInt 
  | TArrow of ty * ty

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


type t =
  | Var of ident
  | Int of int
  | Bool of bool
  | Binop of Op.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of ident * t
  | Apply of t * t


let rec to_string pt =
  match pt with
  | Var v -> v
  | Int i -> Int.to_string i
  | Bool b -> if b then "true" else "false"
  | Binop (op, l, r) ->
    sprintf "%s %s %s" (to_string l) (Op.to_string op) (to_string r)
  | If (cond, if_t, if_f) ->
    sprintf "(if %s then %s else %s)" (to_string cond) (to_string if_t) (to_string if_f)
  | LetIn (name, e1, e2) ->
    sprintf "(let %s = %s in %s)" name (to_string e1) (to_string e2)
  | Fun (arg, body) ->
    sprintf "(fun %s -> %s)" arg (to_string body)
  | Apply (e1, e2) ->
    sprintf "%s %s" (to_string e1) (to_string e2)

(** 
  A top-level command in the program 
*)
type command =
  | Expr of t
  | LetDef of ident * t

let command_to_string = function
  | Expr e -> to_string e
  | LetDef (name, body) ->
    sprintf "let %s = %s ;;" name (to_string body)

type program = command list

let program_to_string prog = 
  List.map command_to_string prog
  |> String.concat "\n"
