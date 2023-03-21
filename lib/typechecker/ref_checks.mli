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
    (as in [let f = fun (x: int[x | x != 0]) -> ...]).

  3.  
    Check that the names in variable signature definitions match
    (as in [val f : int[x | x != 0] -> ...]).

  4. 
    Check that the return variable name doesn't alias any other.
*)

open Ast
open Typing

(**
    Checks that a use-provided type only includes a refinement if 
    if is an annotation on a [Templatetree.Var]    
*)
val check_ref_only_on_var : Templatetree.t -> Ty_template.t -> bool