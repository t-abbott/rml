open Printf
open Utils
open Op
open Typing

module Make (Ty : Ty.Type) = struct
  type 'a astnode = { body : 'a; ty : Ty.t; loc : Location.t }

  type t = t_body astnode
  (** An expression in A-normal form. *)

  and t_body = Let of Ident.t * cexpr | CExpr of cexpr

  and aexpr = aexpr_body astnode
  (** An atomic expression. *)

  and aexpr_body =
    | ANumber of float
    | ABoolean of bool
    | AVar of Ident.t
    | ALambda of Ident.t * t

  and cexpr = cexpr_body astnode
  (** A complex expression. *)

  and cexpr_body =
    | CBinop of Binop.t * aexpr * aexpr
    | CIf of aexpr * aexpr * aexpr
    | CApply of aexpr * aexpr
    | CAexpr of aexpr

  (**
    A top-level expression (either a let-binding or plain variable).
  *)
  type command = LetDef of Ident.t * t | Expr of t

  type program = command list

  let rec aexpr_to_string (ae : aexpr) =
    match ae.body with
    | ANumber n -> Float.to_string n
    | ABoolean b -> Bool.to_string b
    | AVar v -> v
    | ALambda (arg, body) ->
        let ty_str = Ty.to_string ae.ty in
        let body_str = to_string body in
        sprintf "(fun %s -> %s) : %s" arg body_str ty_str

  and cexpr_to_string (ce : cexpr) =
    match ce.body with
    | CBinop (op, l, r) ->
        let op_str = Binop.to_string op in
        let l_str, r_str = Misc.proj2 aexpr_to_string l r in
        sprintf "%s %s %s" l_str op_str r_str
    | CIf (cond, if_t, if_f) ->
        let cond_str = aexpr_to_string cond in
        let if_t_str, if_f_str = Misc.proj2 aexpr_to_string if_t if_f in
        sprintf "if %s then %s else %s" cond_str if_t_str if_f_str
    | CApply (f, x) ->
        let f_str, x_str = Misc.proj2 aexpr_to_string f x in
        sprintf "%s %s" f_str x_str
    | CAexpr ae -> aexpr_to_string ae

  and to_string (e : t) =
    let ty_str = Ty.to_string e.ty in
    match e.body with
    | Let (name, defn) ->
        let defn_str = cexpr_to_string defn in
        sprintf "let %s: %s = %s ;;" name ty_str defn_str
    | CExpr ce ->
        let ce_str = cexpr_to_string ce in
        sprintf "%s: %s" ce_str ty_str

  let command_to_string = function
    | LetDef (name, value) -> sprintf "let %s = %s ;;" name (to_string value)
    | Expr e -> to_string e

  let program_to_string p = List.map command_to_string p |> String.concat "\n"
end
