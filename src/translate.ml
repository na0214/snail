(* snailからCへのトランスレータ *)

open Syntax
open C_lang
open Infer

let rec generate_c_lang_AST_expr term =
  match term with
  | IntLit (i, _) ->
      C_IntLit i
  | FloatLit (f, _) ->
      C_DoubleLit f
  | StringLit (s, _) ->
      C_StringLit s
  | Var (name, uname, _) ->
      if uname != "" then C_Var uname else C_Var name
  | Cons (name, _, _) ->
      C_Var name
  | Prod (sub_term1, sub_term2, _) ->
      C_Pair
        (generate_c_lang_AST_expr sub_term1, generate_c_lang_AST_expr sub_term2)
  | _ ->
      C_StringLit "none"

let generate_c_lang_AST (_ : toplevel) (_ : context) : c_lang_toplevel = []

let translate_to_c toplevel context = context toplevel
