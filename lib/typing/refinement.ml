type t = Var of string | Const of Constant.t

let equal x y = x = y
let to_string = function Var v -> v | Const c -> Constant.to_string c

(** Alias for [Const (Constant.Boolean ...)] *)
let boolean b = Const (Constant.Boolean b)
