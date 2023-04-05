open Ast
open Typing
module Interp_ltree : module type of Interp_anftree.Make (Ty_template)
module PTEnv = Interp_ptree.PTEnv
module TTEnv = Interp_ttree.TTEnv
module LTEnv = Interp_ltree.TEnv

val ptree : Parsetree.program -> PTEnv.envval
val ttree : Templatetree.program -> TTEnv.envval
val ltree : Lineartree.program -> LTEnv.envval
