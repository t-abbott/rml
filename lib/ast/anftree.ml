open Typing.Ty_sig
open Printf
open Utils
open Op

module Make (Ty : TYPE) = struct
  type 'a astnode = { body : 'a; ty : Ty.t; loc : Location.t }

  let node_of body ty loc = { body; ty; loc }

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

  let rec aexpr_to_string (ae : aexpr) =
    match ae.body with
    | ANumber n -> Float.to_string n
    | ABoolean b -> Bool.to_string b
    | AVar v -> Ident_core.to_string v
    | ALambda (arg, body) ->
        let ty_str = Ty.to_string ae.ty in
        let body_str = to_string body in
        let arg_str = List.map Ident_core.to_string arg |> String.concat " " in
        sprintf "(fun %s -> %s): %s" arg_str body_str ty_str

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
    | CApply (fn, params) ->
        let fn_str = aexpr_to_string fn in
        let params_str = List.map aexpr_to_string params |> String.concat " " in
        sprintf "%s %s" fn_str params_str
    | CAexpr ae -> aexpr_to_string ae

  and to_string (e : t) =
    let ty_str = Ty.to_string e.ty in
    match e.body with
    | Let (name, defn, rest) ->
        let name_str = Ident_core.to_string name in
        let defn_str = cexpr_to_string defn in
        let rest_str = to_string rest in
        sprintf "let %s: %s = %s in\n%s" name_str ty_str defn_str rest_str
    | CExpr ce ->
        let ce_str = cexpr_to_string ce in
        sprintf "%s" ce_str

  let command_to_string = function
    | LetDef (name, value) ->
        let ty_str = Ty.to_string value.ty in
        let name_str = Ident_core.to_string name in
        sprintf "let %s: %s = %s" name_str ty_str (to_string value)
    | Expr e -> to_string e

  let program_to_string p =
    List.map command_to_string p |> String.concat " ;;\n\n"

  let t_of_cexpr (ce : cexpr) : t = node_of (CExpr ce) ce.ty ce.loc

  let t_of_aexpr (ae : aexpr) : t =
    node_of (CAexpr ae) ae.ty ae.loc |> t_of_cexpr

  let t_of_bool b ty loc = t_of_aexpr (node_of (ABoolean b) ty loc)
  let t_of_number n ty loc = t_of_aexpr (node_of (ANumber n) ty loc)
  let var v ty loc = t_of_aexpr (node_of (AVar v) ty loc)
end
