open Core
open Base
open Parser.Api
open Typechecker
open Utils

let version = Utils.Version.version

let main filename =
  try
    parse_file filename
    |> (fun p -> Infer.type_program p Utils.Context.empty)
    |> Ast.Typedtree.program_to_string |> Stdio.prerr_endline
  with Typechecker.Infer.TypeError (message, loc) ->
    let loc_str =
      match Location.to_string loc with
      | "" -> ""
      | helpfulstring -> " in " ^ helpfulstring
    in
    Out_channel.eprintf "Type error: %s%s\n" message loc_str;
    Caml.exit 1

(* let main filename =
   try
     parse_file filename
     |> (fun prog ->
       Stdio.print_endline (Parsetree.program_to_string prog);
       Interp.run prog Interp.Env.empty)
     |> Interp.Env.to_string
     |> Stdio.print_endline
   with
     Interp.InterpError (message, loc) ->
       let loc_str = match (Location.to_string loc) with
       | "" -> ""
       | helpfulstring -> " in " ^ helpfulstring
     in
       Out_channel.eprintf "Error: %s%s\n" message loc_str;
       Caml.exit 1
*)

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"summary here"
    ~readme:(fun () -> "a readme? crazy")
    (Command.Param.map filename_param ~f:(fun filename () -> main filename))

let () = Command_unix.run ~version ~build_info:"RWO" command
