open Utils

(**
  Represents the source of a type.         
*)
type t =
  | Builtin  (** the type belongs to a builtin operator/function *)
  | Inferred  (** the type was inferred during type inference *)
  | Annotation of Location.t
      (** the type was annotated by the programmer in-line *)
  | ValStmt of Location.t
      (** the type was annotated by the programmer in a [val] statement  *)
