open Printf
open Utils
open Utils.Ident_sig
module Loc = Location

module Make =
functor
  (Id : IDENT)
  ->
  struct
    module R = Refinement.Make (Id)

    type t = { body : t_body; source : Source.t }
    and t_body = RBase of R.t | RArrow of Id.t * t * t

    type context = t Context.t

    let rec to_string ty =
      match ty.body with
      | RBase r -> R.to_string r
      | RArrow (x, s, t) ->
          let x_str = Id.to_string x in
          let s_str, t_str = Misc.proj2 to_string s t in
          sprintf "%s:%s -> %s" x_str s_str t_str

    let rec is_equal t1 t2 ~with_refinements =
      match (t1.body, t2.body) with
      | RBase r1, RBase r2 ->
          Base_ty.equal r1.base r2.base
          && if with_refinements then R.equal r1 r2 else true
      | RArrow (_, s1, t1), RArrow (_, s2, t2) ->
          is_equal s1 s2 ~with_refinements && is_equal t1 t2 ~with_refinements
      | _ -> false

    let hd ty =
      match ty.body with RArrow (_, s, _) -> Some s | RBase _ -> None

    let tl ty =
      match ty.body with RArrow (_, _, t) -> Some t | RBase _ -> None

    let equal t1 t2 = is_equal t1 t2 ~with_refinements:true
    let equal_base t1 t2 = is_equal t1 t2 ~with_refinements:false
    let is_function ty = match ty.body with RArrow _ -> true | _ -> false
    let builtin ty = { body = ty; source = Builtin }
    let inferred ty = { body = ty; source = Inferred }
    let annotated ty loc = { body = ty; source = Annotation loc }
    let valstmt ty loc = { body = ty; source = ValStmt loc }

    (**
      Strips the refinements from a type.    
    *)
    let rec unrefined = function
      | RBase r -> RBase { r with pred = None }
      | RArrow (x, s, t) ->
          let s' = { s with body = unrefined s.body } in
          let t' = { t with body = unrefined t.body } in
          RArrow (x, s', t')

    (** 
        Returns the component types of a refinement 
    *)
    let rec flatten (ty : t) =
      match ty.body with
      | RBase _ -> [ ty ]
      | RArrow (_, s, t) -> s :: flatten t

    let rec apply_types ?(keep_refinements = false) f_ty arg_tys =
      match (f_ty.body, arg_tys) with
      | ty, [] ->
          if keep_refinements then Some (inferred ty)
          else Some (inferred (unrefined ty))
      | RArrow (_, s, t), x :: xs ->
          if equal_base s x then apply_types t xs else None
      | _ -> None

    let rec arity ty =
      match ty.body with RBase _ -> 0 | RArrow (_, _, t) -> 1 + arity t

    let rec mk_sub fn v thing (ty : t) =
      (let body' =
        match ty.body with
        | RBase r ->
            if r.vname = v then 
              (* [ty] binds [v] so avoid renaming it *)
              RBase r
            else
              (* rename [v] in the predicate *)
              let r' = { r with pred = Option.map (fn v thing) r.pred } in
              RBase r'
        | RArrow (x, s, t) ->
            if x = v then
              (* [ty] binds [x] from [t] onwards so only rename in [s] *)
              RArrow (x, mk_sub fn v thing s, t)
            else 
              (* rename [v] in [s] and [t] *)
              RArrow (x, mk_sub fn v thing s, mk_sub fn v thing t)
      in

      { ty with body = body' })
      [@@ocamlformat "disable"]

    (**
      [sub v x] performs capture-avoiding substitution of [v] for [x] in [ty]
      *)
    let sub = mk_sub R.P.sub

    (**
      [sub_term t term ty] substitutes the variable [v] for [term] in the predicates contained
      in [ty]
    *)
    let sub_term = mk_sub R.P.sub_term

    (* helpers for constructing base types *)
    let t_bool ?(pred = Some R.P.p_true) vname =
      builtin (RBase { vname; base = Base_ty.TBool; pred })

    let t_num ?(pred = Some R.P.p_true) vname =
      builtin (RBase { vname; base = Base_ty.TInt; pred })

    (* useful aliases for binop type generation *)
    let arg1, arg2 = Misc.proj2 Id.of_string "__arg1" "__arg2"
    let x, y, z = Misc.proj3 Id.of_string "__1" "__2" "__3"

    type ty_builder = ?pred:R.P.t option -> Id.t -> t

    let mk_equal v p : R.P.t =
      let ul = Location.unlocated in
      let open R.P in
      ul (IntOp (InterpOp.Equal, ul (Var v), p))

    let make_binop_ty mk_s1 mk_s2 (mk_t : ty_builder) mk_p =
      let pred = Some (mk_equal z (mk_p x y)) in
      let t = mk_t ~pred z in
      let s1 = mk_s1 arg1 in
      let s2 = mk_s2 arg2 in
      let g = builtin (RArrow (y, s2, t)) in
      builtin (RArrow (x, s1, g))

    let ty_of_op =
      let open Op.Binop in
      function
      | Equal -> make_binop_ty t_num t_num t_bool R.P.p_equal
      | Less -> make_binop_ty t_num t_bool t_bool R.P.p_less
      | Greater -> make_binop_ty t_num t_num t_bool R.P.p_greater
      | Plus -> make_binop_ty t_num t_num t_num R.P.p_add
      | Minus -> make_binop_ty t_num t_num t_num R.P.p_sub
      | Times -> make_binop_ty t_num t_num t_num R.P.p_mult
      | Div -> make_binop_ty t_num t_num t_num R.P.p_div
      | Mod -> make_binop_ty t_num t_num t_num R.P.p_mod
      | And -> make_binop_ty t_num t_num t_num R.P.p_and
      | Or -> make_binop_ty t_num t_num t_num R.P.p_or
  end
