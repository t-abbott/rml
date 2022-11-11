open Utils

type t = Var of Ident.t | Const of Constant.t

let equal x y = x = y
let to_string = function Var v -> v | Const c -> Constant.to_string c

(** Alias for [Const (Constant.Boolean ...)] *)
let boolean b = Const (Constant.Boolean b)

let var v = Var v
