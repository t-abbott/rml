open Rml
open Parser.Api

open Core

let version = Version.version

let main filename = 
  let prog = parse_file filename
  in Interp.run prog Interp.Env.empty
  |> Interp.Env.to_string
  |> print_endline

let filename_param =
  let open Command.Param in 
  anon ("filename" %: string)

let command =
  Command.basic 
    ~summary: "summary here"
    ~readme: (fun () -> "a readme? crazy")
    (Command.Param.map filename_param ~f:(fun filename () ->
        main filename))

let () =
      Command_unix.run ~version ~build_info:"RWO" command
