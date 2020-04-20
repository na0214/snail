(* snailからCへのトランスレータ *)

open Syntax
open C_lang
open Infer

let generate_c_lang_AST (_ : toplevel) (_ : context) : c_lang_toplevel = []

let translate_to_c toplevel context = context toplevel
