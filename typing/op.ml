open Rml.Types
open Rml.Types.Ty

module PTree = Parser.Parsetree

(*
  Operator types are curried until I add multi-argument functions  
*)

let bool_bool = builtin (TArrow (builtin TBool, builtin TBool))
let int_int = builtin (TArrow (builtin TInt, builtin TInt))

let int_int_int = builtin (TArrow (int_int, builtin TInt))
let int_int_bool = builtin (TArrow (int_int, builtin TBool)) 
let bool_bool_bool = builtin (TArrow (bool_bool, builtin TBool))

module Binop = struct 
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

  let signature = function
    | Equal -> int_int_bool
    | Less -> int_int_bool
    | Greater -> int_int_bool 
    | Plus -> int_int_int
    | Minus -> int_int_int
    | Times -> int_int_int
    | And -> bool_bool_bool
    | Or -> bool_bool_bool


  (** TODO: should there be one binop representation across the compiler? *)
  let of_ptreeop = function
    | PTree.Op.Equal -> Equal
    | PTree.Op.Less -> Less
    | PTree.Op.Greater -> Greater 
    | PTree.Op.Plus -> Plus
    | PTree.Op.Minus -> Minus
    | PTree.Op.Times -> Times
    | PTree.Op.And -> And 
    | PTree.Op.Or -> Or
end

(* 
module Unop = struct
  type t = 
    | Not
    | Negative 

  let to_string = function 
    | Not -> "!"
    | Negative -> "-"

  let signature = function 
    | Not -> bool_bool 
    | Negative -> int_int 
end *)