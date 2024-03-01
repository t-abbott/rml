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

  val mk_var : Id.t -> t_body
  (**
    [mk_var v = Var v]    
   *)

  val mk_int : int -> t_body
  (**
    [mk_int i = Int i]
   *)

  val mk_bool : bool -> t_body
  (**
    [mk_bool b = Bool b] 
   *)

  val make_equal : t_body -> t_body -> t
  (**
    [make_equal l r] makes a predicate asserting [l = r]        
    *)

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

  val sub_term : Id.t -> t_body -> t -> t
end
