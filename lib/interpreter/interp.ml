module PTEnv = Interp_ptree.PTEnv
module TTEnv = Interp_ttree.TTEnv

let ptree pt = Interp_ptree.run pt Env.empty
let ttree tt = Interp_ttree.run tt Env.empty
