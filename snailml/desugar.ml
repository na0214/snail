open Syntax

(* generate lambda-abstractions from let,fun term that have multiple-arguments *)
let make_lambda args term pos type_annot =
  List.fold_left
    (fun acc name -> Fun ([name], "", acc, pos))
    (match type_annot with Some x -> TypeAnnot (term, x) | None -> term)
    (List.rev args)

(* translate let-term that have multi arguments to unary lambda abstractions *)
let rec let_expr_to_unary_function term =
  match term with
  | Let
      (rec_flag, name, _, args, sub_term1, sub_term2, type_annot, pos, let_bind)
    ->
      Let
        ( rec_flag
        , name
        , ""
        , args
        , make_lambda args (let_expr_to_unary_function sub_term1) pos type_annot
        , let_expr_to_unary_function sub_term2
        , type_annot
        , pos
        , List.map let_expr_to_unary_function let_bind )
  | MutLetBind (name, _, args, sub_term, type_annot, pos) ->
      MutLetBind
        ( name
        , ""
        , args
        , make_lambda args (let_expr_to_unary_function sub_term) pos type_annot
        , type_annot
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
  | TypeAnnot (sub_term, typ) ->
      TypeAnnot (let_expr_to_unary_function sub_term, typ)
  | _ ->
      term

let rec translate_unary_function term =
  match term with
  | LetDec (rec_f, name, args, sub_term, type_annot, pos, let_bind) ->
      LetDec
        ( rec_f
        , name
        , args
        , make_lambda args (let_expr_to_unary_function sub_term) pos type_annot
        , type_annot
        , pos
        , List.map translate_unary_function let_bind )
  | _ ->
      term

let desugar toplevel = List.map translate_unary_function toplevel
