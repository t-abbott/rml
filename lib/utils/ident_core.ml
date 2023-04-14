open Printf

type t = Var of string | Sym of string option * int

let equal = ( = )

let to_string = function
  | Var v -> "var_" ^ v
  | Sym (s, i) ->
      let prefix_str = match s with Some p -> p ^ "_" | None -> "" in
      sprintf "sym_%s%s" prefix_str (Int.to_string i)

let fresh =
  let i = ref 0 in
  let f ?(prefix = "") () =
    let prefix_inner = if prefix <> "" then Some prefix else None in
    incr i;
    Sym (prefix_inner, !i)
  in
  f

let var v = Var v
let orig = function Var v -> v | Sym _ as id -> to_string id
