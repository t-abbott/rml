open Utils

type source = Builtin | Inferred | Annotation of Location.t

type t = { body : t_body; source : source }
and t_body = TInt | TBool | TArrow of t * t

let rec equal t1 t2 =
  match (t1.body, t2.body) with
  | TArrow (x, f), TArrow (y, g) -> equal x y && equal f g
  | x, y -> x = y

let rec to_string (ty : t) =
  match ty.body with
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) -> to_string t1 ^ " -> " ^ to_string t2

let is_base ty = match ty.body with TInt | TBool -> true | _ -> false
let is_function ty = match ty.body with TArrow _ -> true | _ -> false
let builtin ty = { body = ty; source = Builtin }
let inferred ty = { body = ty; source = Inferred }
let annotated ty loc = { body = ty; source = Annotation loc }

(**
  Determine the resulting type after applying a list of arguments
  to a [TArrow].

  Returns [None] in the event of a type error
*)
let rec apply_args f args =
  match (f.body, args) with
  | t, [] -> Some (inferred t)
  | TArrow (t, g), arg :: rest ->
      if t.body = arg.body then apply_args g rest else None
  | _ -> None