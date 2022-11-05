module type SYNTAXTREE = sig
  type t
  type program
  type command

  val to_string : t -> string
  val command_to_string : command -> string
  val program_to_string : program -> string
end
