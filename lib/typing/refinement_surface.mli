(**
  [Refinement_surface] contains the initial representation of
  refinements before semantic checks and type inference have
  been applied.      
*)

open Utils
open Refop

type t_expr = t_expr_body Location.located

and t_expr_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t_expr * t_expr
  | IfThen of t_expr * t_expr * t_expr

type t = { bound_var : Ident.t; expr : t_expr }

val to_string : t -> string

val var : string -> t_expr_body
(** [var v] is an alias for [Var v]*)

val number : int -> t_expr_body
(** [number n] is an alias for [Const (Constant.Integer n)]*)

val boolean : bool -> t_expr_body
(**
    [boolean b] is an alias for [Const (Constant.Boolean b)] 
*)

val refinement : Ident.t -> t_expr -> t
(**
    [refinment v e] is an alias for [{bound_var=v; expr=e}]     
*)
