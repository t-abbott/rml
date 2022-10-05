open Core

val main : unit -> unit
(** Entry point of the interpreter *)

val filename_param : string Command.Param.t
(** The name of the file to be parsed *)

val command : Command.t
(** Command-line argument parser *)