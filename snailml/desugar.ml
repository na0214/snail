open Syntax
open Builtin

(* generate lambda-abstractions from let,fun term that have multiple-arguments *)
let make_lambda args term pos type_annot =
  List.fold_left
    (fun acc name -> Fun ([name], "", acc, pos))
    (match type_annot with Some x -> TypeAnnot (term, x) | None -> term)
    (List.rev args)

(* translate let-term that have multi arguments to unary lambda abstractions *)
let rec let_expr_to_unary_function = function
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
  | App (App (Var (name, _, _), term1), term2)
    when List.mem name builtin_value_constructor ->
      Cons
        ( name
        , Some
            (Prod
               ( let_expr_to_unary_function term1
               , let_expr_to_unary_function term2
               , get_pos_info_term term1 ))
        , get_pos_info_term term1 )
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
  | If (cond, then_t, else_t, pos) ->
      Match
        ( cond
        , [ ( Cons ("True", None, get_pos_info_term then_t)
            , let_expr_to_unary_function then_t )
          ; ( Cons ("False", None, get_pos_info_term else_t)
            , let_expr_to_unary_function else_t ) ]
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
  | ExpMod term ->
      ExpMod (let_expr_to_unary_function term)
  | term ->
      term

let rec translate_unary_function = function
  | LetDec (rec_f, name, args, sub_term, type_annot, pos, let_bind) ->
      LetDec
        ( rec_f
        , name
        , args
        , make_lambda args (let_expr_to_unary_function sub_term) pos type_annot
        , type_annot
        , pos
        , List.map translate_unary_function let_bind )
  | term ->
      term

let desugar toplevel = List.map translate_unary_function toplevel
