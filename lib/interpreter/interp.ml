module PTEnv = Interp_ptree.PTEnv

let ptree pt = Interp_ptree.run pt Env.empty
