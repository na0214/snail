let print_error pos err =
  Core.fprintf stderr "File \"%s\", line %d, position %d:\nError %s\n"
    pos.Syntax.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    err

let parse_with_error lexbuf =
  try Parser.snail_parse Lexer.token lexbuf with
  | Syntax.SyntaxError msg ->
      Core.fprintf stderr "%a:\n%s\n" Lexer.print_position lexbuf msg ;
      exit (-1)
  | Parser.Error ->
      Core.fprintf stderr "%a:\nError: syntax error\n" Lexer.print_position
        lexbuf ;
      exit (-1)

(* print type context *)
let print_context ctx =
  List.iter
    (fun (name, sc) ->
      print_string (name ^ " : " ^ Typedef.print_scheme sc ^ "\n"))
    ( List.filter
        (fun (_, sc) -> not (sc = Typedef.Forall (TyCon (Tycon "None"))))
        (* remove jank type definition *)
        ctx
    |> List.rev )

let parse in_chan =
  let lexbuf = Lexing.from_channel in_chan in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  parse_with_error lexbuf

let processing input_files =
  let in_chan =
    if List.length input_files = 0 then stdin else open_in (List.hd input_files)
  in
  let toplevel = parse in_chan in
  let desugared_ast = Desugar.desugar toplevel in
  let renamed_ast = Rename.rename_toplevel desugared_ast in
  let adt_context =
    Adt.generate_adt_context (Builtin.builtin_typedef @ renamed_ast)
  in
  let type_ctx =
    Infer.typeof_toplevel renamed_ast
      (Builtin.builtin_type_context @ adt_context)
  in
  print_context type_ctx ;
  (*let output = Py_backend.translate_snail_to_python renamed_ast in
  let oc = if output_file = "" then stdout else open_out output_file in
  Core.fprintf oc "%s\n" output ;
  close_out oc ; *)
  close_in in_chan
