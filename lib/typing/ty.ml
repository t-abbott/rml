(**
  {1 TODO}
  - implement [flatten] and [normalise] functions
*)

open Utils
open Base

(** Work around [Base] overriding [=] to have type [int -> int] *)
let ( = ) = Poly.( = )

type source = Builtin | Inferred | Annotation of Location.t

type t = { body : t_body; source : source }
and t_body = TInt | TBool | TArrow of t list * t

(**
  Test if two types are equal.

  [equal t1 t2] matches equivalent function types that have been
  constructed in different ways. For example, a function
  [f : int -> int -> int] could be represented (eliding location tags) 
  as both [TArrow ([TInt], TArrow ([TInt], TInt))], and [TArrow ([TInt; TInt], TInt)].
*)
let rec equal t1 t2 =
  match (t1.body, t2.body) with
  | TArrow (t1_from, t1_to), TArrow (t2_from, t2_to) -> (
      match (t1_from, t2_from) with
      | x :: t1_rest, y :: t2_rest ->
          if equal x y then
            let new_t1 = { t1 with body = TArrow (t1_rest, t1_to) } in
            let new_t2 = { t2 with body = TArrow (t2_rest, t2_to) } in
            equal new_t1 new_t2
          else false
      | [], _ -> equal t1_to t2
      | _, [] -> equal t1 t2_to)
  | x, y -> x = y

let rec to_string (ty : t) =
  match ty.body with
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (tys_from, ty_to) ->
      let args = List.map ~f:to_string tys_from |> String.concat ~sep:" -> " in
      args ^ " -> " ^ to_string ty_to

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
let rec apply_types f types =
  match (f.body, types) with
  | ty, [] -> Some (inferred ty)
  | TArrow ([ ty_param ], g), ty_arg :: args_rest ->
      if equal ty_param ty_arg then apply_types g args_rest else None
  | TArrow (ty_param :: params_rest, g), ty_arg :: args_rest ->
      if equal ty_param ty_arg then
        let new_f = { f with body = TArrow (params_rest, g) } in
        apply_types new_f args_rest
      else None
  | _ -> None
