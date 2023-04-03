open Typing.Ty_sig
open Utils
open Op

(**
  Creates an ANF syntax tree from a type [Ty]. 
*)
module Make : functor (Ty : TYPE) -> sig
  type 'a astnode = { body : 'a; ty : Ty.t; loc : Location.t }

  val node_of : 'a -> Ty.t -> Location.t -> 'a astnode

  type t = t_body astnode
  (** An expression in A-normal form. *)

  and t_body = Let of Ident_core.t * cexpr * t | CExpr of cexpr

  and aexpr = aexpr_body astnode
  (** An atomic expression. *)

  and aexpr_body =
    | ANumber of float
    | ABoolean of bool
    | AVar of Ident_core.t
    | ALambda of Ident_core.t list * t

  and cexpr = cexpr_body astnode
  (** A complex expression. *)

  and cexpr_body =
    | CBinop of Binop.t * aexpr * aexpr
    | CIf of aexpr * aexpr * aexpr
    | CApply of aexpr * aexpr list
    | CAexpr of aexpr

  (**
      A top-level expression (either a let-binding or plain variable).
    *)
  type command = LetDef of Ident_core.t * t | Expr of t

  type program = command list

  val to_string : t -> string
  val command_to_string : command -> string
  val program_to_string : program -> string
end
