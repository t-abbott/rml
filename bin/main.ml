open Core
open Base
open Out_channel
open Errors
open Parser.Api
open Typechecker
open Interpreter
open Lowering
open Utils

let version = Utils.Version.version
let die () = Caml.exit 1

let format_location loc =
  match Location.to_string loc with
  | "" -> ""
  | helpfulstring -> " in " ^ helpfulstring

(* hsdf *)
let check_vcs (vc, loc) =
  match Smt.solve vc with
  | Smt.SAT -> ()
  | Smt.UNSAT ->
      let msg = "unsatisfiable refinement" in
      raise (UnsatError (msg, loc))
  | Smt.UNKNOWN ->
      let msg = "unknown result to refinement" in
      raise (UnsatError (msg, loc))

let main filename =
  try
    (* parse, type, and lower to ANF *)
    let ltree =
      parse_file filename
      |> (fun p ->
           Log.log
             (sprintf "\n\ninput parsetree:\n%s"
                (Ast.Parsetree.program_to_string p));
           p)
      |> (fun p -> Infer.type_program p [])
      |> Anf.anf_program
    in
    Log.log
      (sprintf "\nANF'd to:\n%s\n" (Ast.Lineartree.program_to_string ltree));

    (* generate VCs for liquid type checking *)
    let vcs = Vc.check_program [] ltree in

    (* check vc types *)
    List.iter vcs ~f:check_vcs;

    Interp.ltree ltree |> Interp.LTEnv.to_string |> ( ^ ) "\n"
    |> Stdio.print_endline
  with
  | Parser.Errors.ParseError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nParser error: %s%s\n" message loc_str;
      die ()
  | Parser.Errors.LexError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nLexer error: %s%s\n" message loc_str;
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
  | UnsatError (message, loc) ->
      let loc_str = format_location loc in
      eprintf "\nType error: %s%s\n" message loc_str;
      die ()

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"summary here"
    ~readme:(fun () -> "a readme? crazy")
    (Command.Param.map filename_param ~f:(fun filename () -> main filename))

let () = Command_unix.run ~version ~build_info:"RWO" command
