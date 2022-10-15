open Printf

type t = {
  line: int;
  column: int;
  file: string option
}

let from ?(file=None) line column = { line; column; file }

let to_string { line; column; file } =
  let file_str = match file with
  | None -> ""
  | Some name -> "; " ^ name   
  in sprintf "[line %s; column %s%s]" (Int.to_string line) (Int.to_string column) file_str