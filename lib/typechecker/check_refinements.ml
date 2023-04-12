open Printf
open Typing
open Errors
open Utils
module Ty = Ty_template

type context = Ty.t Context.t

let tys_of_op op =
  let open Ty.R.P.InterpOp in
  let open Base_ty in
  match op with
  | Equal -> (TInt, TInt, TBool)
  | Less -> (TInt, TInt, TBool)
  | Greater -> (TInt, TInt, TBool)
  | Add -> (TInt, TInt, TInt)
  | Sub -> (TInt, TInt, TInt)
  | Mult -> (TInt, TInt, TInt)
  | Div -> (TInt, TInt, TInt)
  | Mod -> (TInt, TInt, TInt)

(**
  [check_predicate p ctx tb] checks that a predicate p has type [tb] wrt a context [ctx]
*)
let rec check_predicate (pred : Ty.R.P.t) (ctx : context) ty_expected =
  let open Ty.R.P in
  let ty_actual =
    match pred.body with
    | Var v -> (
        match Context.find v ctx with
        | Some { body = RBase r; _ } -> r.base
        | None ->
            let msg =
              sprintf "reference to unknown variable '%s' in refinement" v
            in
            raise (RefinementError (msg, pred.loc))
        | _ ->
            let msg =
              sprintf "expected '%s' to have base sort, found functinon" v
            in
            raise (RefinementError (msg, pred.loc)))
    | Int _ -> TInt
    | Bool _ -> TBool
    | IntOp (op, l, r) ->
        let ty_l_e, ty_r_e, ty_res_e = tys_of_op op in
        ignore (check_predicate l ctx ty_l_e);
        ignore (check_predicate r ctx ty_r_e);

        ty_res_e
    | Conj (l, r) | Disj (l, r) ->
        ignore (check_predicate l ctx TBool);
        ignore (check_predicate r ctx TBool);
        TBool
    | IfThen (cond, if_t, if_f) ->
        ignore (check_predicate cond ctx TBool);
        ignore (check_predicate if_t ctx ty_expected);
        ignore (check_predicate if_f ctx ty_expected);

        ty_expected
  in
  if Base_ty.equal ty_actual ty_expected then ()
  else
    let ty_e_str, ty_a_str =
      Misc.proj2 Base_ty.to_string ty_expected ty_actual
    in
    let msg =
      sprintf "expected expression to have type '%s' but got '%s'" ty_e_str
        ty_a_str
    in
    raise (RefinementError (msg, pred.loc))

let rec check_inner (r : Ty.t) (ctx : context) =
  match r.body with
  | Ty.RArrow (x, s, t) ->
      (* check that s is well-formed *)
      ignore (check_inner s ctx);

      (* check that t is well-formed *)
      let ctx' = Context.extend x s ctx in
      ignore (check_inner t ctx');
      r
  | Ty.RBase { vname; pred; _ } -> (
      match pred with
      | Some p ->
          let ctx' = Context.extend vname r ctx in
          ignore (check_predicate p ctx' Base_ty.TBool);
          r
      | None -> r)

let check_ref r = check_inner r Context.empty
