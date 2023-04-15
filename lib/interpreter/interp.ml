open Typing
module PTEnv = Interp_ptree.PTEnv
module TTEnv = Interp_ttree.TTEnv
module Interp_ltree = Interp_anftree.Make (Ty_template)
module LTEnv = Interp_ltree.TEnv

let ptree pt = Interp_ptree.run pt []
let ttree tt = Interp_ttree.run tt []
let ltree lt = Interp_ltree.run lt []
