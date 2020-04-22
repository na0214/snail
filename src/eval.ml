open Syntax

type eval_object =
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Pair of eval_object * eval_object
  | Eval_Func of string * term
  | Eval_Data of eval_object * eval_object
  | Eval_Cons of string
  | Eval_Term of term * string
[@@deriving show]

exception PatternNotMatch

exception RuntimeError of string

type eval_context = (string * eval_object) list [@@deriving show]

let find_eval_context name ctx =
  try List.assoc name ctx
  with Not_found -> RuntimeError ("unbound identifier:" ^ name) |> raise

let rec replace_variable name base_term term =
  match base_term with
  | Let (rec_flag, n, uname, arguments, sub_term1, sub_term2, pos) ->
      Let
        ( rec_flag
        , n
        , uname
        , arguments
        , replace_variable name sub_term1 term
        , replace_variable name sub_term2 term
        , pos )
  | Fun (arguments, n, subterm, pos) ->
      Fun (arguments, n, replace_variable name subterm term, pos)
  | App (sub_term1, sub_term2) ->
      App
        ( replace_variable name sub_term1 term
        , replace_variable name sub_term2 term )
  | Prod (sub_term1, sub_term2, pos) ->
      Prod
        ( replace_variable name sub_term1 term
        , replace_variable name sub_term2 term
        , pos )
  | Match (sub_term, pat_list, pos) ->
      Match
        ( replace_variable name sub_term term
        , List.map (fun (p, t) -> (p, replace_variable name t term)) pat_list
        , pos )
  | Var (n, uname, pos) ->
      if name = n then term else Var (n, uname, pos)
  | s ->
      s

let rec unify_pat pat obj =
  match (pat, obj) with
  | Prod (sub_pat1, sub_pat2, _), Eval_Pair (eval_obj1, eval_obj2) ->
      unify_pat sub_pat1 eval_obj1 @ unify_pat sub_pat2 eval_obj2
  | Cons (n1, _), Eval_Cons n2 ->
      if n1 = n2 then [] else raise PatternNotMatch
  | App (sub_pat1, sub_pat2), Eval_Data (eval_obj1, eval_obj2) ->
      unify_pat sub_pat1 eval_obj1 @ unify_pat sub_pat2 eval_obj2
  | Var (_, uname, _), obj ->
      [(uname, obj)]
  | _ ->
      (*print_string (Syntax.show_term pat ^ " : " ^ show_eval_object obj ^ "\n") ;*)
      raise PatternNotMatch

let rec apply_term term1 term2 ctx =
  match term1 with
  | Eval_Func (argument, subterm) ->
      eval_term (replace_variable argument subterm term2) ctx
  | Eval_Cons cons_name ->
      Eval_Data (Eval_Cons cons_name, eval_term term2 ctx)
  | _ ->
      RuntimeError "cannot apply" |> raise

and pattern_match obj pat_list =
  match pat_list with
  | [] ->
      raise Not_found
  | (pat, t) :: xs -> (
    try
      let new_ctx = unify_pat pat obj in
      eval_term t new_ctx
    with PatternNotMatch -> pattern_match obj xs )

and eval_term term eval_ctx =
  match term with
  | IntLit (n, _) ->
      Eval_Int n
  | FloatLit (f, _) ->
      Eval_Float f
  | StringLit (s, _) ->
      Eval_String s
  | Prod (sub_term1, sub_term2, _) ->
      let fst_obj = eval_term sub_term1 eval_ctx in
      let snd_obj = eval_term sub_term2 eval_ctx in
      Eval_Pair (fst_obj, snd_obj)
  | Fun ([name], _, sub_term, _) ->
      Eval_Func (name, sub_term)
  | App (sub_term1, sub_term2) ->
      apply_term (eval_term sub_term1 eval_ctx) sub_term2 eval_ctx
  | Var (name, uname, _) -> (
      let eval_obj =
        if uname = "" then find_eval_context name eval_ctx
        else find_eval_context uname eval_ctx
      in
      match eval_obj with
      | Eval_Term (t, n) ->
          eval_term t ((n, Eval_Term (t, n)) :: eval_ctx)
      | _ ->
          eval_obj )
  | Cons (name, _) ->
      Eval_Cons name
  | Let (rec_flag, _, uname, _, sub_term1, sub_term2, _) ->
      let sub_term_result =
        if rec_flag then
          eval_term sub_term1 ((uname, Eval_Term (sub_term1, uname)) :: eval_ctx)
        else eval_term sub_term1 eval_ctx
      in
      eval_term sub_term2 ((uname, sub_term_result) :: eval_ctx)
  | Match (sub_term, pat_list, _) ->
      let sub_term_result = eval_term sub_term eval_ctx in
      pattern_match sub_term_result pat_list
  | _ ->
      Eval_String "None"

let eval =
  List.fold_left
    (fun acc x ->
      match x with
      | LetDec (_, name, _, term, _) ->
          let result = eval_term term acc in
          print_string (show_eval_object result ^ "\n") ;
          (name, result) :: acc
      | _ ->
          acc)
    []
