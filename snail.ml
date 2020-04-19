open Lib

let parse_with_error lexbuf =
  try Parser.snail_parse Lexer.token lexbuf with
  | Syntax.SyntaxError msg ->
      Core.fprintf stderr "%a: %s\n" Lexer.print_position lexbuf msg ;
      exit (-1)
  | Parser.Error ->
      Core.fprintf stderr "%a: syntax error\n" Lexer.print_position lexbuf ;
      exit (-1)

let _ =
  (* let in_chan = open_in "example/test.sn" in *)
  let in_chan = stdin in
  let lexbuf = Lexing.from_channel in_chan in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  let ast = parse_with_error lexbuf in
  let type_ = Infer.typeof ast [] in
  print_string (Syntax.show_term ast ^ "\n") ;
  print_string (Typedef.show_snail_type type_ ^ "\n") ;
  close_in in_chan
