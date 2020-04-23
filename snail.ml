open Src

let parse_with_error lexbuf =
  try Parser.snail_parse Lexer.token lexbuf with
  | Syntax.SyntaxError msg ->
      Core.fprintf stderr "%a: %s\n" Lexer.print_position lexbuf msg ;
      exit (-1)
  | Parser.Error ->
      Core.fprintf stderr "%a: syntax error\n" Lexer.print_position lexbuf ;
      exit (-1)

let print_context ctx =
  List.iter
    (fun (name, sc) -> print_string (name ^ Typedef.print_scheme sc ^ "\n"))
    ctx

let _ =
  let in_chan =
    if Array.length Sys.argv = 2 then open_in Sys.argv.(1) else stdin
  in
  let lexbuf = Lexing.from_channel in_chan in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  let toplevel = parse_with_error lexbuf in
  (*print_string (Syntax.show_snail_AST toplevel ^ "\n") ;*)
  let desugared_ast = Desugar.desugar toplevel in
  let renamed_ast = Rename.rename_toplevel desugared_ast in
  let adt_context = Adt.generate_adt_context renamed_ast in
  (*print_string (Infer.show_context adt_context ^ "\n") ;*)
  (*print_string (Syntax.show_snail_AST renamed_ast ^ "\n") ;*)
  let type_ctx = Infer.typeof_toplevel renamed_ast adt_context in
  print_context type_ctx ;
  (*let _ = Eval.eval renamed_ast in
  print_string (Eval.show_eval_context result ^ "\n") ;*)
  close_in in_chan
