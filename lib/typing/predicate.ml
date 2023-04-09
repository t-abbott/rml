open Printf
open Utils
open Utils.Ident_sig

module InterpOp = struct
  type t = Add | Sub | Mult | Div

  let type_of (_ : t) : Ty_template.t = failwith "not implemented"
  let to_string = function Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "%"
end

module Make =
functor
  (Id : IDENT)
  ->
  struct
    type t =
      | Var of Id.t
      | Const of Constant.t
      | IntOp of InterpOp.t * t * t
      | Conj of t * t
      | Disj of t * t
      | IfThen of t * t * t

    let rec to_string = function
      | Var v -> Id.to_string v
      | Const c -> Constant.to_string c
      | IntOp (op, l, r) ->
          let l_str = to_string l in
          let r_str = to_string r in
          sprintf "(%s %s %s)" l_str (InterpOp.to_string op) r_str
      | Conj (l, r) -> sprintf "(%s && %s)" (to_string l) (to_string r)
      | Disj (l, r) -> sprintf "(%s || %s)" (to_string l) (to_string r)
      | IfThen (cond, if_t, if_f) ->
          let c_str, t_str, f_str = Misc.proj3 to_string cond if_t if_f in
          sprintf "(if %s then %s else %s)" c_str t_str f_str
  end
