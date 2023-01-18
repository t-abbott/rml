(**
  [Refinement_surface] contains the initial representation of
  refinements before semantic checks and type inference have
  been applied.      
*)

open Utils
open Refop

type t = t_body Location.located

and t_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t * t
  | IfThen of t * t * t

val to_string : t -> string

val var : string -> t_body
(** [var v] is an alias for [Var v]*)

val number : int -> t_body
(** [number n] is an alias for [Const (Constant.Integer n)]*)

val boolean : bool -> t_body
(**
    [boolean b] is an alias for [Const (Constant.Boolean b)] 
*)
