open Printf
open Utils
open Utils.Ident_sig

module Make =
functor
  (Id : IDENT)
  ->
  struct
    module InterpOp = struct
      type t = Add | Sub | Mult | Div | Mod | Equal | Less | Greater

      let to_string = function
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Equal -> "="
        | Less -> "<"
        | Greater -> ">"
    end

    type t = t_body Location.located

    and t_body =
      | Var of Id.t
      | Bool of bool
      | Int of int
      | IntOp of InterpOp.t * t * t
      | Conj of t * t
      | Disj of t * t
      | IfThen of t * t * t

    let rec to_string ({ body; _ } : t) =
      match body with
      | Var v -> Id.to_string v
      | Bool b -> Bool.to_string b
      | Int i -> Int.to_string i
      | IntOp (op, l, r) ->
          let l_str = to_string l in
          let r_str = to_string r in
          sprintf "(%s %s %s)" l_str (InterpOp.to_string op) r_str
      | Conj (l, r) -> sprintf "(%s && %s)" (to_string l) (to_string r)
      | Disj (l, r) -> sprintf "(%s || %s)" (to_string l) (to_string r)
      | IfThen (cond, if_t, if_f) ->
          let c_str, t_str, f_str = Misc.proj3 to_string cond if_t if_f in
          sprintf "(if %s then %s else %s)" c_str t_str f_str

    (**
    Substitutes [v] for [x] in [p]
  *)
    let rec sub v x (p : t) =
      let f = sub v x in
      let new_body =
        match p.body with
        | Var u -> Var (if u <> v then u else x)
        | IntOp (op, l, r) -> IntOp (op, f l, f r)
        | Conj (l, r) -> Conj (f l, f r)
        | Disj (l, r) -> Disj (f l, f r)
        | IfThen (cond, if_t, if_f) -> IfThen (f cond, f if_t, f if_f)
        | _ -> p.body
      in
      { p with body = new_body }
  end
