open Rml.Types

module Binop : sig 
    type t =
      | Equal 
      | Less
      | Greater
      | Plus 
      | Minus
      | Times 
      | And
      | Or
 
    (**  *)
    val to_string : t -> string
   
    (** jksf *)
    val signature : t -> Ty.t

    val of_ptreeop : Parser.Parsetree.Op.t -> t
end