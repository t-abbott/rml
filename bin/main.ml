open Core
open Base
open Out_channel
open Parser.Api
open Typechecker
open Interpreter
open Utils

let version = Utils.Version.version
let die () = Caml.exit 1

let format_location loc =
  match Location.to_string loc with
  | "" -> ""
  | helpfulstring -> " in " ^ helpfulstring

let main filename =
  try
    parse_file filename
    |> (fun p -> Infer.type_program p Utils.Context.empty)
    |> Interp.ttree |> Interp.TTEnv.to_string |> Stdio.prerr_endline
  with
  | Parser.Errors.ParseError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "Parser error: %s%s\n" message loc_str;
      die ()
  | Parser.Errors.LexError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "Error while lexing: %s%s\n" message loc_str;
      die ()
  | Typechecker.Errors.TypeError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "Type error: %s%s\n" message loc_str;
      die ()
  | Typechecker.Errors.NameError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "Error: %s%s\n" message loc_str;
      die ()

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"summary here"
    ~readme:(fun () -> "a readme? crazy")
    (Command.Param.map filename_param ~f:(fun filename () -> main filename))

let () = Command_unix.run ~version ~build_info:"RWO" command
