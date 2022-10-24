module Ty = struct
  type t = t_body Location.located
  and t_body = 
    | TInt
    | TBool 
    | TArrow of t * t

  let rec to_string (ty: t) = 
      match ty.body with
      | TInt -> "int"
      | TBool -> "bool"
      | TArrow (t1, t2) ->
        (to_string t1) ^ " -> " ^ (to_string t2)
end