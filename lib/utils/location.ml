open Printf

type t =
  | Loc of {
      line_start : int;
      line_end : int;
      char_start : int;
      char_end : int;
      filename : string option;
    }
  | Nowhere

type 'a located = { loc : t; body : 'a }

let locate loc body = { loc; body }
let unlocated body = { loc = Nowhere; body }

let from start_pos end_pos =
  let line_start = start_pos.Lexing.pos_lnum in
  let line_end = end_pos.Lexing.pos_lnum in
  let char_start = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
  let char_end = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
  let filename = Some start_pos.Lexing.pos_fname in
  Loc { line_start; line_end; char_start; char_end; filename }

let to_string = function
  | Nowhere -> ""
  | Loc loc ->
      let filestring =
        match loc.filename with Some name -> name | None -> "[unknown]"
      in
      if loc.line_start = loc.line_end then
        sprintf "file \"%s\" line %d character %d-%d" filestring loc.line_start
          loc.char_start loc.char_end
      else
        sprintf "file \"%s\" line %d character %d to line %d character %d"
          filestring loc.line_start loc.char_start loc.line_end loc.char_end
