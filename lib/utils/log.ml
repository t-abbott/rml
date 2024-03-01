let stdout = Stdlib.stderr
let stderr = Stdlib.stderr
let target = ref None
let set_output output = target := output

let log msg =
  let open Stdlib in
  match !target with
  | Some outfile ->
      output_string outfile (msg ^ "\n");
      flush outfile
  | None -> ()
