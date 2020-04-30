open Syntax
open Python_lang
open Typedef
open Infer

exception CompileError of string

let match_py =
  "\n\
   def match(pat1, pat2):\n\
   \tif (type(pat2) is dict) and (type(pat1) is dict):\n\
   \t\tif list(pat2.keys())[0] == list(pat1.keys())[0]:\n\
   \t\t\treturn match(pat2[list(pat2.keys())[0]], pat1[list(pat1.keys())[0]])\n\
   \t\telse:\n\
   \t\t\treturn False\n\
   \telif (type(pat2) is tuple) and (type(pat1) is tuple):\n\
   \t\t_f = True\n\
   \t\tfor _i, _t in enumerate(pat2):\n\
   \t\t\t_f = match(pat1[_i],_t) and _f\n\
   \t\treturn _f\n\
   \telif (type(pat1) is str) and (pat1[0] == \"m\"):\n\
   \t\tglobals()[pat1] = pat2\n\
   \t\treturn True\n\
   \telif (type(pat2) is str) and (pat2[0] == \"m\"):\n\
   \t\tglobals()[pat2] = pat1\n\
   \t\treturn True\n\
   \telif pat1 == None and pat2 == None:\n\
   \t\treturn True\n\
   \telse:\n\
   \t\treturn False\n"

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
  | PyTerm_If (cond, t_term, f_term) ->
      "(("
      ^ py_code_generate_term t_term
      ^ ") if (" ^ py_code_generate_term cond ^ ") else ("
      ^ py_code_generate_term f_term
      ^ "))"
  | PyTerm_Match_If (match_t, pat, t_term, f_term) ->
      "(("
      ^ py_code_generate_term t_term
      ^ ") if (" ^ "match("
      ^ py_code_generate_term match_t
      ^ "," ^ py_code_generate_term pat ^ ")) else ("
      ^ py_code_generate_term f_term
      ^ "))"
  | PyTerm_Error ->
      "exit()"
  | PyTerm_None ->
      "None"

let py_code_generate (pcode : py_code) : string =
  List.fold_left
    (fun acc top ->
      match top with
      | Bind (local_flag, name, py_term) when local_flag ->
          acc ^ "def " ^ name ^ "(_ctx):\n" ^ "\treturn ("
          ^ py_code_generate_term py_term
          ^ ")\n"
      | Bind (_, name, py_term) ->
          acc ^ "def " ^ name ^ "():\n" ^ "\treturn ("
          ^ py_code_generate_term py_term
          ^ ")\n"
      | TopLet (name, py_term) ->
          acc ^ name ^ " = " ^ py_code_generate_term py_term ^ "\n"
      | _ ->
          "")
    "" pcode

let rec translate_term_to_python term ctx inner_pat =
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
      else if uname.[0] = 'm' && inner_pat then PyTerm_Var ("\"" ^ uname ^ "\"")
      else if uname.[0] = 'm' then PyTerm_Var uname
      else PyTerm_App (PyTerm_Var uname, PyTerm_Dict ctx)
    with Not_found -> PyTerm_Var (n ^ "()") )
  | Prod (sub_term1, sub_term2, _) ->
      PyTerm_Tuple
        ( translate_term_to_python sub_term1 ctx inner_pat
        , translate_term_to_python sub_term2 ctx inner_pat )
  | Fun (_, uname, sub_term, _) ->
      PyTerm_Lambda
        ( uname
        , translate_term_to_python sub_term
            ((uname, PyTerm_Var uname) :: ctx)
            inner_pat )
  | Cons (n, term_opt, _) -> (
    match term_opt with
    | Some x ->
        PyTerm_Dict [(n, translate_term_to_python x ctx inner_pat)]
    | None ->
        PyTerm_Dict [(n, PyTerm_None)] )
  | App (sub_term1, sub_term2) ->
      PyTerm_App
        ( translate_term_to_python sub_term1 ctx inner_pat
        , translate_term_to_python sub_term2 ctx inner_pat )
  | Match (sub_term, pat_list, _) ->
      let r =
        List.fold_right
          (fun (pat, t) acc ->
            let new_ctx =
              ctx
              @ List.map
                  (fun n -> (n, PyTerm_Var n))
                  (get_pattern_var_unique pat)
            in
            PyTerm_Match_If
              ( translate_term_to_python sub_term ctx inner_pat
              , translate_term_to_python pat new_ctx true
              , translate_term_to_python t new_ctx inner_pat
              , acc ))
          pat_list PyTerm_Error
      in
      print_string (show_py_term r ^ "\n") ;
      r
  | Let (_, _, uname, _, _, sub_term, _) ->
      translate_term_to_python sub_term
        ((uname, PyTerm_Var uname) :: ctx)
        inner_pat

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
  | PyTerm_If (cond, t_term, f_term) ->
      PyTerm_If
        ( replace_ctx_variable cond ctx
        , replace_ctx_variable t_term ctx
        , replace_ctx_variable f_term ctx )
  | PyTerm_Match_If (cond_1, cond_2, t_term, f_term) ->
      PyTerm_Match_If
        ( replace_ctx_variable cond_1 ctx
        , replace_ctx_variable cond_2 ctx
        , replace_ctx_variable t_term ctx
        , replace_ctx_variable f_term ctx )
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
      let body =
        replace_ctx_variable (translate_term_to_python sub_term ctx false) ctx
      in
      Bind (true, name, body) :: generate_local_var_func sub_term2 new_ctx
  | Fun (_, name, sub_term, _) ->
      let new_ctx = (name, PyTerm_Var name) :: ctx in
      generate_local_var_func sub_term new_ctx
  | App (t1, t2) ->
      generate_local_var_func t1 ctx @ generate_local_var_func t2 ctx
  | Match (sub_term, pat_list, _) ->
      generate_local_var_func sub_term ctx
      @ List.fold_left
          (fun acc (pat, t) ->
            (*let new_ctx =
              ctx
              @ List.map
                  (fun n -> (n, PyTerm_Var n))
                  (get_pattern_var_unique pat)
            in*)
            acc
            @ generate_local_var_func pat ctx
            @ generate_local_var_func t ctx)
          [] pat_list
  | _ ->
      []

let rec generate_pattern_var term =
  match term with
  | Let (_, _, _, _, sub_term1, sub_term2, _) ->
      generate_pattern_var sub_term1 @ generate_pattern_var sub_term2
  | Fun (_, _, sub_term, _) ->
      generate_pattern_var sub_term
  | App (sub_term1, sub_term2) ->
      generate_pattern_var sub_term1 @ generate_pattern_var sub_term2
  | Prod (sub_term1, sub_term2, _) ->
      generate_pattern_var sub_term1 @ generate_pattern_var sub_term2
  | Match (sub_term, pat_list, _) ->
      generate_pattern_var sub_term
      @ List.fold_left
          (fun acc (pat, t) ->
            acc
            @ List.map
                (fun n -> TopLet (n, PyTerm_None))
                (get_pattern_var_unique pat)
            @ generate_pattern_var t)
          [] pat_list
  | _ ->
      []

let translate_snail_to_python (ast : snail_AST) : string =
  List.fold_left
    (fun acc top ->
      match top with
      | LetDec (_, name, _, term, _) ->
          let pattern_ctx = generate_pattern_var term in
          let local_ctx = generate_local_var_func term [] in
          let new_bind =
            Bind (false, name, translate_term_to_python term [] false)
          in
          let result_str =
            py_code_generate (pattern_ctx @ local_ctx @ [new_bind])
          in
          print_string (match_py ^ "\n") ;
          acc ^ result_str
      | _ ->
          acc)
    "" ast
  ^ match_py ^ "\nif __name__ == \"__main__\":\n\tprint(main())\n"

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
