open Typing
module PTEnv = Interp_ptree.PTEnv
module TTEnv = Interp_ttree.TTEnv
module Interp_ltree = Interp_anftree.Make (Ty_template)

let ptree pt = Interp_ptree.run pt Env.empty
let ttree tt = Interp_ttree.run tt Env.empty
let ltree lt = Interp_ltree.run lt Interp_ltree.TEnv.empty
