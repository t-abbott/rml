open Base

type 'a t = (string * 'a) list

(*
  TODO: make this a functor to contexts and environments
  can share the same code?   
*)

let empty = []

let find name ctx = List.Assoc.find ctx name ~equal:(String.equal)

let extend name value env = (name, value) :: env