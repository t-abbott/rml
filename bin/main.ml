open Core
open Base
open Out_channel
open Parser.Api
open Typechecker
open Interpreter

(* open Lowering *)
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
    |> (fun p -> Infer.type_program p [])
    |> Interp.ttree |> Interp_ttree.TTEnv.to_string
    (* |> Anf.anf_program |> Interp.ltree |> Interp.LTEnv.to_string |> ( ^ ) "\n" *)
    |> Stdio.print_endline
  with
  | Parser.Errors.ParseError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nParser error: %s%s\n" message loc_str;
      die ()
  | Parser.Errors.LexError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nError while lexing: %s%s\n" message loc_str;
      die ()
  | Typechecker.Errors.TypeError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nType error: %s%s\n" message loc_str;
      die ()
  | Typechecker.Errors.NameError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nError: %s%s\n" message loc_str;
      die ()
  | Typechecker.Errors.RefinementError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nRefinement error: %s%s\n" message loc_str;
      die ()
  | Interpreter.Errors.InterpError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nRuntime error: %s%s\n" message loc_str;
      die ()
  | Typing.Refinement_errors.RefinementError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nRefinement error: %s%s\n" message loc_str;
      die ()

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"summary here"
    ~readme:(fun () -> "a readme? crazy")
    (Command.Param.map filename_param ~f:(fun filename () -> main filename))

let () = Command_unix.run ~version ~build_info:"RWO" command
