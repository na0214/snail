open Typedef

let builtin_type_context =
  [ ("()", Forall (TyCon (Tycon "()")))
  ; ( "+"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Int")) )
  ; ( "-"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Int")) )
  ; ( "*"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Int")) )
  ; ( "/"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Int")) )
  ; ( ">="
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Bool"))
    )
  ; ( "<="
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Bool"))
    )
  ; ( "<"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Bool"))
    )
  ; ( ">"
    , Forall
        (TyCon (Tycon "Int") @-> TyCon (Tycon "Int") @-> TyCon (Tycon "Bool"))
    )
  ; ("==", Forall (TyGen 0 @-> TyGen 0 @-> TyCon (Tycon "Bool")))
  ; ("print_string", Forall (TyCon (Tycon "String") @-> TyCon (Tycon "()")))
  ; ("print_int", Forall (TyCon (Tycon "Int") @-> TyCon (Tycon "()")))
  ; ("print_float", Forall (TyCon (Tycon "Float") @-> TyCon (Tycon "()"))) ]

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

let builtin_function =
  "\ndef print_string():\n\treturn (lambda x:print(x,end=\"\"))\n"
  ^ "\ndef print_int():\n\treturn (lambda x:print(x,end=\"\"))\n"
  ^ "\ndef print_float():\n\treturn (lambda x:print(x,end=\"\"))\n"

let builtin_binop_name =
  ["+()"; "-()"; "*()"; "/()"; "<()"; ">()"; "==()"; "<=()"; ">=()"]
