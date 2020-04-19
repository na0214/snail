(* C言語の生成に用いる関数など *)

type c_type = C_Int | C_Double | C_Char | C_Pointer of c_type

type c_argument = string * c_type

type c_lang = CFunc of c_argument list * c_type * c_lang
