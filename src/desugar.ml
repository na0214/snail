open Syntax

let make_lambda args term pos =
  List.fold_left
    (fun acc name -> Fun ([name], "", acc, pos))
    term (List.rev args)

let rec let_expr_to_unary_function term =
  match term with
  | Let (rec_flag, name, _, args, sub_term1, sub_term2, pos) ->
      Let
        ( rec_flag
        , name
        , ""
        , args
        , make_lambda args (let_expr_to_unary_function sub_term1) pos
        , let_expr_to_unary_function sub_term2
        , pos )
  | Fun (arg, name, sub_term, pos) ->
      Fun (arg, name, let_expr_to_unary_function sub_term, pos)
  | App (sub_term1, sub_term2) ->
      App
        ( let_expr_to_unary_function sub_term1
        , let_expr_to_unary_function sub_term2 )
  | Cons (name, Some t, pos) ->
      Cons (name, Some (let_expr_to_unary_function t), pos)
  | Prod (sub_term1, sub_term2, pos) ->
      Prod
        ( let_expr_to_unary_function sub_term1
        , let_expr_to_unary_function sub_term2
        , pos )
  | Match (sub_term, pat_list, pos) ->
      Match
        ( let_expr_to_unary_function sub_term
        , List.map
            (fun (pat, t) -> (pat, let_expr_to_unary_function t))
            pat_list
        , pos )
  | _ ->
      term

let desugar_term = let_expr_to_unary_function

let translate_unary_function term =
  match term with
  | LetDec (rec_f, name, args, sub_term, pos) ->
      LetDec
        (rec_f, name, args, make_lambda args (desugar_term sub_term) pos, pos)
  | _ ->
      term

let desugar = List.map translate_unary_function
