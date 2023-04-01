(**
  How do we check that the name of the bound var in a refinement
  matches the name of the underlying variable it refines?
 
  1.
    Check that refinements are only applied to variables 
    (otherwise the idea of a refinement binding to a name
     doesn't make sense).

    We need to to this on both [PTree.Annotated] and [PTree.LetDef].     

  2.
    Check that the names in inline definition annotations match
    (as in [let f = fun (x: num[x | x != 0]) -> ...]).

    We do this inline when lowering [PTree.Fun]s and [PTree.LetIn]s
    
  3.  
    Check that the names in variable signature definitions match
    (as in [
        val f : num[x | x != 0] -> ...
        let f = (fun (x: num[x | x != 0]) -> ...)
    ]).

    We do this inline when lowering [PTree.Fun]s. 

  4. 
    Check that the return variable name doesn't alias any other.

    We do this inline when lowering [PTree.Fun]s.
*)

open Ast
open Typing

val check_ref_only_on_var : Templatetree.t -> Ty_template.t -> bool
(**
    Checks that a use-provided type only includes a refinement if 
    if is an annotation on a [Templatetree.Var]    
*)

val check_inline_var_names_match : Templatetree.t -> Ty_template.t -> bool
(**
    Checks if the name bound in a refinement matches the variable 
    the refinement is attached to 
    
    (e.g. rejects expressions like [x: num[y | y < 0]])
*)

val check_var_matches_bound_var : string -> Ty_surface.t -> bool
(**
    TODO docstring 
*)

val check_ref_annotations :
  Templatetree.t -> Ty_template.t -> (unit, string) result
(**
    TODO docstring    
*)
