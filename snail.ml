open Src

let parse_with_error lexbuf =
  try Parser.snail_parse Lexer.token lexbuf with
  | Syntax.SyntaxError msg ->
      Core.fprintf stderr "%a: %s\n" Lexer.print_position lexbuf msg ;
      exit (-1)
  | Parser.Error ->
      Core.fprintf stderr "%a: syntax error\n" Lexer.print_position lexbuf ;
      exit (-1)

let _ =
  (*let in_chan = open_in "examples/parametric.sn" in*)
  let in_chan = stdin in
  let lexbuf = Lexing.from_channel in_chan in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  let toplevel = parse_with_error lexbuf in
  let desugared_ast = Desugar.desugar toplevel in
  let renamed_ast = Rename.rename_toplevel desugared_ast in
  print_string (Syntax.show_snail_AST renamed_ast ^ "\n") ;
  (*let ctx = Infer.typeof_toplevel desugared_ast Infer.empty_context in
  print_string (Infer.show_context ctx ^ "\n") ;*)
  close_in in_chan
