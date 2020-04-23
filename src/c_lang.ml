(* C言語の生成に用いる関数など *)

type c_type = C_Type of string | C_Pointer of c_type [@@deriving show]

type c_argument = string * c_type [@@deriving show]

type c_lang_expr_term =
  | C_IntLit of int
  | C_DoubleLit of float
  | C_StringLit of string
  | C_Var of string
  | C_App of c_lang_expr_term * c_lang_expr_term
  | C_Pair of c_lang_expr_term * c_lang_expr_term
[@@deriving show]

type c_lang_statement =
  | C_Let of c_type * string * c_lang_expr_term
  | C_Return of c_lang_expr_term
[@@deriving show]

type c_lang =
  | C_Func of c_type * string * c_argument list * c_lang_statement list
[@@deriving show]

type c_lang_toplevel = c_lang list [@@deriving show]
