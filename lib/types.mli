module Ty : sig
  type source = Builtin | Inferred | Annotation of Location.t

  type t = { body : t_body; source : source }
  and t_body = TInt | TBool | TArrow of t * t

  val equal : t -> t -> bool
  val to_string : t -> string
  val is_base : t -> bool
  val is_function : t -> bool
end

val builtin : Ty.t_body -> Ty.t
val inferred : Ty.t_body -> Ty.t
val annotated : Ty.t_body -> Location.t -> Ty.t
val apply_args : Ty.t -> Ty.t list -> Ty.t option
