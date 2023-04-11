open Utils
open Utils.Ident_sig

module Make : functor (Id : IDENT) -> sig
  module InterpOp : sig
    type t = Add | Sub | Mult | Div | Mod | Equal | Less | Greater

    val to_string : t -> string
  end

  type t = t_body Location.located

  and t_body =
    | Var of Id.t
    | Bool of bool
    | Int of int
    | IntOp of InterpOp.t * t * t
    | Conj of t * t
    | Disj of t * t
    | IfThen of t * t * t

  val p_true : t
  (**
    Alias for the unlocated predicate true
  *)

  val p_false : t
  (**
    Alias for the unlocated predicate false
  *)

  val p_add : Id.t -> Id.t -> t
  val p_sub : Id.t -> Id.t -> t
  val p_mult : Id.t -> Id.t -> t
  val p_div : Id.t -> Id.t -> t
  val p_equal : Id.t -> Id.t -> t
  val p_less : Id.t -> Id.t -> t
  val p_greater : Id.t -> Id.t -> t
  val p_mod : Id.t -> Id.t -> t
  val p_and : Id.t -> Id.t -> t
  val p_or : Id.t -> Id.t -> t
  val to_string : t -> string

  val sub : Id.t -> Id.t -> t -> t
  (**
        [sub v x p] substitutes [v] for [x] in [p]    
    *)
end
