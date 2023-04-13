open Ast
open Typing
open Utils

val impl_constraint : Ident.t -> Ty_template.t -> Constraint.t -> Constraint.t
(**
    [impl_constraint x t c] synthesises a constraint [c] that corresponds to the
    implication constraint such that [t] holds under [x] (if [t] is basic) implies
    that [c] holds. 
*)

val sub : Ty_template.t -> Ty_template.t -> Constraint.t
(**
    [sub t1 t2] synthesises a constraint [c] such that [c] holds implies that
    [t1] is a subtype of [t2].    
*)

val synth : Ty_template.context -> Lineartree.t -> Constraint.t * Ty_template.t
(**
    [synth ctx e] synthesises a pair [(c, t')] for the expression [e] such that
    [c] is a constraint that implies that the refined type [t'] is valid.
*)

val check : Ty_template.context -> Lineartree.t -> Ty_template.t -> Constraint.t
(**
    [check ctx e t] synthesises a vc [c] such that [c] whose validity implies that
    [e] checks against [t].
*)

val check_program :
  Ty_template.context -> Lineartree.program -> Constraint.t list
