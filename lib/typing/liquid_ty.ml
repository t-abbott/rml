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
          Base_ty.equal r1.base r2.base && with_refinements && R.equal r1 r2
      | RArrow (_, s1, t1), RArrow (_, s2, t2) ->
          is_equal s1 s2 ~with_refinements && is_equal t1 t2 ~with_refinements
      | _ -> false

    let equal t1 t2 = is_equal t1 t2 ~with_refinements:true
    let equal_base t1 t2 = is_equal t1 t2 ~with_refinements:false
    let is_function ty = match ty.body with RArrow _ -> true | _ -> false
    let builtin ty = { body = ty; source = Builtin }
    let inferred ty = { body = ty; source = Inferred }
    let annotated ty loc = { body = ty; source = Annotation loc }
    let valstmt ty loc = { body = ty; source = ValStmt loc }

    (* returns the component types of a refinement *)
    let rec flatten (ty : t) =
      match ty.body with
      | RBase _ -> [ ty ]
      | RArrow (_, s, t) -> s :: flatten t

    let rec arity ty =
      match ty.body with RBase _ -> 0 | RArrow (_, _, t) -> 1 + arity t
  end
