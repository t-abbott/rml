open Printf
open Utils
module Ref = Refinement.Make (Ident_core)

type t = RBase of Ref.t | RArrow of Ident_core.t * t * t

let rec to_string = function
  | RBase r -> Ref.to_string r
  | RArrow (x, s, t) ->
      let x_str = Ident_core.to_string x in
      let s_str, t_str = Misc.proj2 to_string s t in
      sprintf "%s:%s -> %s" x_str s_str t_str
