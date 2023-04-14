open Printf

type t = Var of string | GenSym of string option * int | BuiltinSym of string

let equal = ( = )

let to_string = function
  | Var v -> "var_" ^ v
  | GenSym (s, i) ->
      let prefix_str = match s with Some p -> p ^ "_" | None -> "" in
      sprintf "sym_%s%s" prefix_str (Int.to_string i)
  | BuiltinSym s -> sprintf "builtin_%s" s

let fresh =
  let i = ref 0 in
  let f ?(prefix = "") () =
    let prefix_inner = if prefix <> "" then Some prefix else None in
    incr i;
    GenSym (prefix_inner, !i)
  in
  f

let var v = Var v

let of_other = function
  | Var v -> fresh ~prefix:v ()
  | s -> fresh ~prefix:(to_string s) ()

let orig = function
  | Var v -> v
  | GenSym _ as id -> to_string id
  | BuiltinSym s -> s
