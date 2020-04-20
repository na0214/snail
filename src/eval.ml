open Syntax

type eval_object =
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Func of string * term
[@@deriving show]

exception RuntimeError of string

type eval_context = (string * eval_object) list [@@deriving show]

let find_eval_context name ctx =
  try List.assoc name ctx
  with Not_found -> RuntimeError ("unbound identifier:" ^ name) |> raise

let rec replace_variable name base_term term =
  match base_term with
  | Let (n, uname, arguments, sub_term1, sub_term2, pos) ->
      Let
        ( n
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
  | Var (n, uname, pos) ->
      if name = n then term else Var (n, uname, pos)
  | s ->
      s

let rec apply_term term1 term2 ctx =
  match term1 with
  | Eval_Func (argument, subterm) ->
      eval_term (replace_variable argument subterm term2) ctx
  | _ ->
      RuntimeError "cannot apply" |> raise

and eval_term term eval_ctx =
  match term with
  | IntLit (n, _) ->
      Eval_Int n
  | FloatLit (f, _) ->
      Eval_Float f
  | StringLit (s, _) ->
      Eval_String s
  | Fun ([name], _, sub_term, _) ->
      Eval_Func (name, sub_term)
  | App (sub_term1, sub_term2) ->
      apply_term (eval_term sub_term1 eval_ctx) sub_term2 eval_ctx
  | Var (_, name, _) ->
      find_eval_context name eval_ctx
  | _ ->
      Eval_String "None"

let eval toplevel =
  let _ =
    List.map
      (fun top ->
        match top with
        | LetDec (_, _, term, _) ->
            let result = eval_term term [] in
            print_string (show_eval_object result ^ "\n") ;
            result
        | _ ->
            Eval_String "")
      toplevel
  in
  []
