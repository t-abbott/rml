module Ty: sig 
  type t = t_body Location.located
  and t_body = 
    | TInt
    | TBool 
    | TArrow of t * t
  
  val to_string : t -> string
end
