open Printf

type ident = string

module Op = struct
  type t = 
    | Equals
    | Nequals 
    | Less 
    | Greater
    | Add
    | Sub 
    | Mult
    | Div
    | And
    | Or 

  let ops = [ 
    (Equals, "=");
    (Nequals, "!=");
    (Less, "<");
    (Greater, ">");
    (Add, "+");
    (Sub, "-");
    (Mult, "*");
    (Div, "/");
    (And, "&&");
    (Or, "||");
  ]
  
  let to_string op =
    List.assoc op ops

  let of_string s =
    List.assoc s (Utils.inverse ops)
end


type term =  
  | True
  | False
  | Int of int 
  | Var of ident
  | Binop of Op.t * term * term
  | IfThen of term * term * term
  | LetIn of ident * term * term
  | Fun of ident * term
  | App of term * term

let rec to_string = function
  | True -> "true"
  | False -> "false"
  | Var v -> v
  | IfThen (cond, ift, iff) ->
      sprintf "if %s then %s else %s" (to_string cond) (to_string ift) (to_string iff)
  | Int i -> Int.to_string i
  | Binop (op, l, r) ->
    sprintf "(%s %s %s)" (to_string l) (Op.to_string op) (to_string r)
  | LetIn (v, e, body) ->
    sprintf "let %s = %s in %s" v (to_string e) (to_string body)
  | Fun (arg, body) ->
    sprintf "(fun %s -> %s)" arg (to_string body)
  | App (e1, e2) ->
    sprintf "(%s %s)" (to_string e1) (to_string e2)

(** 
  Represents a definition at the top level of a program.
  Top-level definitions have public scope and are terminated
  by a double semicolon [";;"] 
*)
module Defn = struct
  type t = 
    | TypeDefn of ident * unit
    | LetDefn of ident * term    
    | Expr of term

  let to_string = function
    | TypeDefn (name, _) -> 
      sprintf "type %s = () ;;\n" name
    | LetDefn (name, body) ->
       sprintf "let %s = %s ;;\n" name (to_string body)
    | Expr e -> to_string e
end

(**
  An `rml` program is a list of definitions.
*)
type program = Defn.t list

let run prog = 
  List.map Defn.to_string prog
  |> String.concat ""
