open Base
open Printf
open Ast
open Ast.Templatetree
open Typing
open Utils
open Errors

(* open Ref_checks *)
module PTree = Ast.Parsetree
module TTree = Ast.Templatetree
module L = Location

type context = Ty_template.t Context.t

let t_int = Ty_template.unrefined Base_ty.TInt ~source:Builtin
let t_bool = Ty_template.unrefined Base_ty.TBool ~source:Builtin

(* let check_annotatated_refs expr ty =
   match check_ref_annotations expr ty with
   | Ok () -> ()
   | Error msg -> raise (TypeError (msg, expr.loc)) *)

(**
  Check that ...
 *)
let check_tys_match loc ~ty_stated ~ty_inferred =
  match ty_stated with
  | Some stated ->
      if not (Ty_template.equal_base stated ty_inferred) then
        let ty_i_str, ty_s_str =
          Misc.proj2 Ty_template.to_string ty_inferred stated
        in
        let msg =
          sprintf "inferred type '%s' doesn't match stated type '%s'" ty_i_str
            ty_s_str
        in
        raise (TypeError (msg, loc))
      else ()
  | None -> ()

(**
  unfold [ty] returns a list of all the component types of [ty]. 
  e.g. [unfold (a -> b -> b)] ~> [[(a -> b -> c); (b -> c); c]]    
*)
let rec unfold (ty : Ty_template.t) =
  match ty.body with
  | Ty_template.RBase _ -> [ ty ]
  | Ty_template.RArrow (_, t) -> ty :: unfold t

(**
  [pair_args xs tys] pairs a list of parameter idents with a list
  of types appearing a function signature ([length tys] should be one
  greater than [length xs]).    
*)
let rec pair_args xs tys =
  match (xs, tys) with
  | y :: ys, t :: tys -> (
      match pair_args ys tys with
      | Some tl -> Some ((y, t) :: tl)
      | None -> None)
  | [], [ _ ] -> Some []
  | _ -> None

(**
  [curry xs e loc] creates a curried function with the body
  [e] and parameters [xs] at loction [loc].  
*)
let curry params body loc =
  List.fold_right params
    ~f:(fun (x, ty) body -> TTree.from (Fun ([ x ], body)) ty loc)
    ~init:body

let rec type_parsetree ?(ty_stated = None) (pt : PTree.t) ctx =
  Stdlib.prerr_endline "\ncalled type_parsetree";

  let loc = pt.loc in
  match pt.body with
  | PTree.Number i ->
      let body = Number i in
      let ty = t_int in
      { body; ty; loc }
  | PTree.Boolean b ->
      let body = Boolean b in
      let ty = t_bool in
      { body; ty; loc }
  | PTree.Var v ->
      let ty =
        match Context.find v ctx with
        | Some ty' -> ty'
        | None ->
            (* TODO: handle this in a semantic analysis pass on the parsetree? *)
            let msg = sprintf "reference to unknown variable '%s'" v in
            raise (NameError (msg, loc))
      in
      let body = Var v in
      { body; ty; loc }
  | PTree.Binop (op, l, r) -> (
      let ty_op = Op.Binop.signature op in
      let l', r' = (type_parsetree l ctx, type_parsetree r ctx) in
      let body = Binop (op, l', r') in

      match Ty_template.apply_types ty_op [ l'.ty; r'.ty ] with
      | Some ty -> { body; ty; loc }
      | None ->
          let msg =
            sprintf
              "arguments of type '%s' and '%s' did not match expected type \
               '%s' for operator '%s'"
              (Ty_template.to_string l'.ty)
              (Ty_template.to_string r'.ty)
              (Ty_template.to_string ty_op)
              (Op.Binop.to_string op)
          in
          raise (TypeError (msg, loc)))
  | PTree.If (cond, ift, iff) ->
      let typed_cond = type_parsetree cond ctx in
      let typed_ift, typed_iff =
        (type_parsetree ift ctx, type_parsetree iff ctx)
      in
      let body = If (typed_cond, typed_ift, typed_iff) in
      let ty = typed_ift.ty in
      if not (Ty_template.equal typed_cond.ty t_bool) then
        let t_str = Ty_template.to_string typed_cond.ty in
        let msg =
          sprintf "expected if statement condition to be a bool, got '%s'" t_str
        in
        raise (TypeError (msg, loc))
      else if not (Ty_template.equal typed_ift.ty typed_iff.ty) then
        let s1, s2 =
          ( Ty_template.to_string typed_ift.ty,
            Ty_template.to_string typed_iff.ty )
        in
        let msg =
          sprintf "expected branches to have the same type, got '%s', '%s" s1 s2
        in
        raise (TypeError (msg, loc))
      else { body; ty; loc }
  | PTree.LetIn (name, value, rest) ->
      Stdlib.prerr_endline "in LetIn";

      (* check if a type for [name] as already been stated
         if so, add it to the context
      *)
      let val_ty = Context.find name ctx in
      let val_ty_str =
        match val_ty with
        | Some ty -> Ty_template.to_string ty
        | None -> "<empty>"
      in
      Stdlib.prerr_endline (sprintf "name: %s found val: %s" name val_ty_str);

      (* infer a type for the typed body *)
      let typed_value = type_parsetree ~ty_stated:val_ty value ctx in

      (* check that the inferred base type matches the stated base type *)
      let _ =
        match val_ty with
        | Some stated_ty ->
            if not (Ty_template.equal_base stated_ty typed_value.ty) then
              let ty_i_str, ty_s_str =
                Misc.proj2 Ty_template.to_string typed_value.ty stated_ty
              in
              let msg =
                sprintf "inferred type '%s' doesn't match stated type '%s'"
                  ty_i_str ty_s_str
              in
              raise (TypeError (msg, pt.loc))
            else ()
        | None -> ()
      in

      (* add the type of [name] to the context *)
      let ty = typed_value.ty in
      let ctx' = Context.extend name ty ctx in

      (* check the inferred expression *)
      let typed_rest = type_parsetree rest ctx' in

      let body = LetIn (name, typed_value, typed_rest) in
      { body; ty; loc }
  | PTree.ValIn (name, ty, rest) ->
      Stdlib.prerr_endline "in ValIn";
      let ty_t = Ty_template.of_surface ty ctx in
      let ctx' = Context.extend name ty_t ctx in
      Stdlib.print_endline
        (sprintf "ValIn %s : %s" name (Ty_template.to_string ty_t));

      (* then process the rest of the tree *)
      type_parsetree rest ctx'
  | PTree.Fun (params, body) ->
      Stdlib.prerr_endline "in Fun";

      (* check that we have an expected type of the function through a valdef *)
      let val_ty =
        match ty_stated with
        | Some ty -> ty
        | None ->
            let msg =
              "expected function signature to be provided in a val expression"
            in
            raise (TypeError (msg, pt.loc))
      in

      (* 1. match the first n args of the function with the types at their stage of the signature *)
      let fn_tys = unfold val_ty in
      let param_ty_pairs =
        match pair_args params fn_tys with
        | Some pairs -> pairs
        | None ->
            let n_defn = List.length params in
            let n_sig = List.length fn_tys - 1 in
            let msg =
              sprintf
                "number of params in function definition and signature don't \
                 match - got %d, expected %d"
                n_defn n_sig
            in
            raise (TypeError (msg, pt.loc))
      in

      (* 2. then add the arguments to the context and check the type of the body *)
      let ctx' =
        List.fold param_ty_pairs ~init:ctx ~f:(fun c (x, ty) ->
            Context.extend x ty c)
      in
      let typed_body = type_parsetree body ctx' in

      (* 3. test the base type of the typed function body matches that defined in the signature *)
      let ty_final =
        match List.rev fn_tys |> List.hd with
        | Some t -> t
        | _ ->
            (* we check that fn_tys has enough elements 2 sections prior *)
            failwith "unreachable"
      in
      if not (Ty_template.equal ty_final typed_body.ty) then
        let t_val_str, t_inf_str =
          Misc.proj2 Ty_template.to_string ty_final typed_body.ty
        in
        let msg =
          sprintf
            "stated type of function body '%s' doesnt match inferred type '%s'"
            t_val_str t_inf_str
        in
        raise (TypeError (msg, body.loc))
      else
        (* 4. produced a curried version of the multi-argument function *)
        curry param_ty_pairs typed_body pt.loc
  | PTree.Apply (e1, e2s) ->
      let typed_e1, typed_e2s =
        (type_parsetree e1 ctx, List.map ~f:(fun e -> type_parsetree e ctx) e2s)
      in
      if Ty_template.is_function typed_e1.ty then
        let body = Apply (typed_e1, typed_e2s) in
        let ty =
          let arg_tys = List.map ~f:(fun e -> e.ty) typed_e2s in

          (* test we're applying the right number of arguments *)
          let n_params = Ty_template.arity typed_e1.ty in
          let n_args = List.length e2s in
          if n_args <> n_params then
            let msg =
              sprintf
                "attemped to apply %d arguments to a function of %d parameters"
                n_args n_params
            in
            raise (TypeError (msg, loc))
          else
            (* test argument types match *)
            match Ty_template.apply_types typed_e1.ty arg_tys with
            | Some t -> t
            | None ->
                let f_ty_str = Ty_template.to_string typed_e1.ty in
                let arg_ty_strs =
                  List.map ~f:(fun e -> Ty_template.to_string e.ty) typed_e2s
                in
                let msg =
                  sprintf
                    "attempted to apply arguments of type '%s' to a function \
                     of type '%s'"
                    f_ty_str
                    (String.concat ~sep:" " arg_ty_strs)
                in
                raise (TypeError (msg, loc))
        in
        { body; ty; loc }
      else
        let s1 = Ty_template.to_string typed_e1.ty in
        let msg =
          sprintf "attempted to apply to a non-function value of type '%s'" s1
        in
        raise (TypeError (msg, loc))
  | PTree.Annotated (expr, ty_stated) ->
      let ty_t_stated = Ty_template.of_surface ty_stated ctx in
      let typed_expr = type_parsetree expr ctx in

      (* check any refinement annotation is well-formed *)
      (* ignore (check_annotatated_refs typed_expr ty_t_stated); *)
      if not (Ty_template.equal_base typed_expr.ty ty_t_stated) then
        let stated, inferred =
          Utils.Misc.proj2 Ty_template.to_string ty_t_stated typed_expr.ty
        in
        let msg =
          sprintf "provided type '%s' does not match inferred type '%s'" stated
            inferred
        in
        raise (TypeError (msg, loc))
      else { typed_expr with ty = ty_t_stated }

let type_command (cmd : PTree.command) (ctx : context) :
    TTree.command option * context =
  match cmd.body with
  | PTree.Expr ptree ->
      let ttree = type_parsetree ptree ctx in
      (Some (Expr ttree), ctx)
  | PTree.ValDef (name, ty) ->
      (* extend the context with the val annotation and move on *)
      let ty_t = Ty_template.of_surface ty ctx in
      (None, Context.extend name ty_t ctx)
  | PTree.LetDef (name, defn) ->
      (* test if a type has been provided in a prior [val] statement *)
      let val_ty = Context.find name ctx in

      (* infer the type of [defn] *)
      let defn_t = type_parsetree ~ty_stated:val_ty defn ctx in

      (* test the inferred type matches the val-defined type *)
      ignore (check_tys_match cmd.loc ~ty_stated:val_ty ~ty_inferred:defn_t.ty);

      (* otherwise the expression is well-typed and we can add it to the environment *)
      let ctx' = Context.extend name defn_t.ty ctx in
      (Some (LetDef (name, defn_t)), ctx')

let rec type_program prog ctx =
  match prog with
  | cmd :: rest -> (
      match type_command cmd ctx with
      | Some typed_cmd, ctx' -> typed_cmd :: type_program rest ctx'
      | None, ctx' -> type_program rest ctx')
  | [] -> []
