open Printf
open Utils
open Utils.Ident_sig
module L = Location

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

    let make_intop op l r =
      let ul = L.unlocated in
      ul (IntOp (op, ul l, ul r))

    let mk_var x = Var x
    let mk_int i = Int i
    let mk_bool b = Bool b
    let make_equal = make_intop InterpOp.Equal
    let p_true : t = L.unlocated (Bool true)
    let p_false : t = L.unlocated (Bool false)

    let make_binop op x y : t =
      L.unlocated (IntOp (op, L.unlocated (mk_var x), L.unlocated (mk_var y)))

    let p_add = make_binop InterpOp.Add
    let p_sub = make_binop InterpOp.Sub
    let p_mult = make_binop InterpOp.Mult
    let p_div = make_binop InterpOp.Div
    let p_equal = make_binop InterpOp.Equal
    let p_less = make_binop InterpOp.Less
    let p_greater = make_binop InterpOp.Greater
    let p_mod = make_binop InterpOp.Mod

    let p_and x y =
      L.unlocated (Conj (L.unlocated (mk_var x), L.unlocated (mk_var y)))

    let p_or x y =
      L.unlocated (Disj (L.unlocated (mk_var x), L.unlocated (mk_var y)))

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

    let rec sub_term v term (p : t) =
      let f = sub_term v term in
      let body' =
        match p.body with
        | Var u -> if v = u then term else Var u
        | IntOp (op, l, r) -> IntOp (op, f l, f r)
        | Conj (l, r) -> Conj (f l, f r)
        | Disj (l, r) -> Disj (f l, f r)
        | IfThen (cond, if_t, if_f) -> IfThen (f cond, f if_t, f if_f)
        | _ -> p.body
      in

      { p with body = body' }
  end
