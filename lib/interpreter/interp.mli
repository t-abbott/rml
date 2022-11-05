open Ast
module PTEnv = Interp_ptree.PTEnv

val ptree : Parsetree.program -> PTEnv.envval