open Utils

type t = Builtin | Inferred | Annotation of Location.t | ValStmt of Location.t
