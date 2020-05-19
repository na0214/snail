open Snailml

let _ =
  let input_files = ref [] in
  let output_file = ref "" in
  let arguments_spec =
    [("-o", Arg.String (fun str -> output_file := str), "Output file")]
  in
  Arg.parse_argv Sys.argv arguments_spec
    (fun str -> input_files := str :: !input_files)
    "" ;
  try Processing.processing !input_files !output_file
  with Infer.TypeError (err, pos) -> Processing.print_error pos err
