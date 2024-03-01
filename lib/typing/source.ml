open Utils

type t = Builtin | Inferred | Annotation of Location.t | ValStmt of Location.t

let to_string = function
  | Builtin -> "builtin"
  | Inferred -> "inferred"
  | Annotation at -> "annotation @ " ^ Location.to_string at
  | ValStmt at -> "val @ " ^ Location.to_string at
