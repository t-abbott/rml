open Utils

exception InterpError of string * Location.t

let unreachable ~reason ~loc =
  let message = "tried to execute branch unreachable because " ^ reason in
  raise (InterpError (message, loc))
