open Syntax

type eval_object =
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Func of term
[@@deriving show]

exception RuntimeError of string

type eval_context = (string * eval_object) list [@@deriving show]

let find_eval_context name ctx =
  try List.assoc name ctx
  with Not_found -> RuntimeError ("unbound identifier:" ^ name) |> raise

let eval_term term eval_ctx =
  match term with
  | IntLit (n, _) ->
      Eval_Int n
  | FloatLit (f, _) ->
      Eval_Float f
  | StringLit (s, _) ->
      Eval_String s
  | Fun (_, sub_term, _) ->
      Eval_Func sub_term
  (*| App (sub_term1, sub_term2) ->
      apply_term sub_term1 (eval_term sub_term2)*)
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
