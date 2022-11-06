open Ast
module PTEnv = Interp_ptree.PTEnv
module TTEnv = Interp_ttree.TTEnv

val ptree : Parsetree.program -> PTEnv.envval
val ttree : Typedtree.program -> TTEnv.envval