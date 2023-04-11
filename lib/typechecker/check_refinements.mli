open Typing

val check_ref : Ty_template.t -> Ty_template.t
(**
  [check_ref ty] checks that the refinements of the liquid type [ty]
  are well-formed and raises a [RefinementError] they aren't (otherwise
  returns [ty] untouched). 
*)
