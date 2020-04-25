open Syntax
open Python_lang
open Infer
open Typedef

exception CompileError of string

let rec py_code_generate_term py_term =
  match py_term with
  | PyTerm_Int i ->
      string_of_int i
  | PyTerm_Float f ->
      string_of_float f
  | PyTerm_String s ->
      "\"" ^ s ^ "\""
  | PyTerm_Var v ->
      v
  | PyTerm_Lambda (name, sub_term) ->
      "lambda " ^ name ^ ":" ^ py_code_generate_term sub_term
  | PyTerm_App (sub_term1, sub_term2) ->
      "("
      ^ py_code_generate_term sub_term1
      ^ ")" ^ " ("
      ^ py_code_generate_term sub_term2
      ^ ")"
  | PyTerm_Tuple (sub_term1, sub_term2) ->
      "("
      ^ py_code_generate_term sub_term1
      ^ ","
      ^ py_code_generate_term sub_term2
      ^ ")"
  | PyTerm_Dict dict_l ->
      "{"
      ^ List.fold_left
          (fun acc (name, sub_term) ->
            acc ^ "\'" ^ name ^ "\':" ^ py_code_generate_term sub_term ^ ",")
          "" dict_l
      ^ "}"

let py_code_generate (pcode : py_code) : string =
  List.fold_left
    (fun acc top ->
      match top with
      | Bind (_, name, py_term) when name = "main" ->
          acc ^ "if __name__ == \"__main__\":\n\t"
          ^ py_code_generate_term py_term
          ^ "\n"
      | Bind (local_flag, name, py_term) when local_flag ->
          acc ^ "def " ^ name ^ "(_ctx):\n" ^ "\treturn ("
          ^ py_code_generate_term py_term
          ^ ")\n"
      | Bind (local_flag, name, py_term) when not local_flag ->
          acc ^ "def " ^ name ^ "():\n" ^ "\treturn ("
          ^ py_code_generate_term py_term
          ^ ")\n"
      | _ ->
          "")
    "" pcode

let rec translate_term_to_python term ctx =
  match term with
  | IntLit (v, _) ->
      PyTerm_Int v
  | FloatLit (v, _) ->
      PyTerm_Float v
  | StringLit (v, _) ->
      PyTerm_String v
  | Var (n, uname, _) -> (
    try
      let _ = List.assoc uname ctx in
      if uname.[0] = 'f' then PyTerm_Var uname
      else PyTerm_App (PyTerm_Var uname, PyTerm_Dict ctx)
    with Not_found -> PyTerm_Var (n ^ "()") )
  | Prod (sub_term1, sub_term2, _) ->
      PyTerm_Tuple
        ( translate_term_to_python sub_term1 ctx
        , translate_term_to_python sub_term2 ctx )
  | Fun (_, uname, sub_term, _) ->
      PyTerm_Lambda
        ( uname
        , translate_term_to_python sub_term ((uname, PyTerm_Var uname) :: ctx)
        )
  | Cons (n, term_opt, _) -> (
    match term_opt with
    | Some x ->
        translate_term_to_python x ctx
    | None ->
        PyTerm_Var n )
  | App (sub_term1, sub_term2) ->
      PyTerm_App
        ( translate_term_to_python sub_term1 ctx
        , translate_term_to_python sub_term2 ctx )
  | Let (_, _, uname, _, _, sub_term, _) ->
      translate_term_to_python sub_term ((uname, PyTerm_Var uname) :: ctx)
  | _ ->
      exit 0

let rec replace_ctx_variable term ctx =
  match term with
  | PyTerm_Lambda (n, pterm) ->
      PyTerm_Lambda (n, replace_ctx_variable pterm ctx)
  | PyTerm_App (pt1, pt2) ->
      PyTerm_App (replace_ctx_variable pt1 ctx, replace_ctx_variable pt2 ctx)
  | PyTerm_Tuple (pt1, pt2) ->
      PyTerm_Tuple (replace_ctx_variable pt1 ctx, replace_ctx_variable pt2 ctx)
  | PyTerm_Dict ctx_l ->
      PyTerm_Dict
        (List.map (fun (n, pterm) -> (n, replace_ctx_variable pterm ctx)) ctx_l)
  | PyTerm_Var n -> (
    try
      match List.assoc n ctx with
      | PyTerm_Var _ ->
          PyTerm_Var ("_ctx[\"" ^ n ^ "\"]")
      | _ ->
          CompileError "py context is invalid" |> raise
    with Not_found -> PyTerm_Var n )
  | t ->
      t

(* ローカル束縛をトップレベルに展開 *)
let rec generate_local_var_func term ctx =
  match term with
  | Let (_, _, name, _, sub_term, sub_term2, _) ->
      let new_ctx = (name, PyTerm_Var name) :: ctx in
      Bind
        ( true
        , name
        , replace_ctx_variable (translate_term_to_python sub_term ctx) ctx )
      :: generate_local_var_func sub_term2 new_ctx
  | Fun (_, name, sub_term, _) ->
      let new_ctx = (name, PyTerm_Var name) :: ctx in
      generate_local_var_func sub_term new_ctx
  | App (t1, t2) ->
      generate_local_var_func t1 ctx @ generate_local_var_func t2 ctx
  | _ ->
      []

let translate_snail_to_python (ast : snail_AST) : string =
  List.fold_left
    (fun acc top ->
      match top with
      | LetDec (_, name, _, term, _) ->
          let local_ctx = generate_local_var_func term [] in
          let new_bind = Bind (false, name, translate_term_to_python term []) in
          let result_str = py_code_generate (local_ctx @ [new_bind]) in
          acc ^ result_str
      | _ ->
          acc)
    "" ast

let rec is_include_arrow typ =
  match typ with
  | TyCon (Tycon s) when s = "->" ->
      true
  | TyApp (typ1, typ2) ->
      is_include_arrow typ1 || is_include_arrow typ2
  | TyPair (typ1, typ2) ->
      is_include_arrow typ1 || is_include_arrow typ2
  | _ ->
      false

let generate_constructor (adt_ctx : context) : string =
  List.fold_left
    (fun acc (name, Forall typ) ->
      if is_include_arrow typ then acc else acc ^ name ^ " = None\n")
    "" adt_ctx
