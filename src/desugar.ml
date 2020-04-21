open Syntax

let make_lambda args term pos =
  List.fold_left
    (fun acc name -> Fun ([name], "", acc, pos))
    term (List.rev args)

let let_expr_to_unary_function term =
  match term with
  | Let (rec_flag, name, _, args, sub_term1, sub_term2, pos) ->
      Let
        ( rec_flag
        , name
        , ""
        , args
        , make_lambda args sub_term1 pos
        , sub_term2
        , pos )
  | _ ->
      term

let desugar_term = let_expr_to_unary_function

let translate_unary_function term =
  match term with
  | LetDec (name, args, sub_term, pos) ->
      LetDec (name, args, make_lambda args (desugar_term sub_term) pos, pos)
  | _ ->
      term

let desugar = List.map translate_unary_function
