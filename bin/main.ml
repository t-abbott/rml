open Core
open Rml

let version = Rml.Version.version

let main filename = 
  Parser.parse_file filename
  |> Syntax.run
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
