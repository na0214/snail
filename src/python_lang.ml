type py_term =
  | PyTerm_None
  | PyTerm_Int of int
  | PyTerm_Float of float
  | PyTerm_String of string
  | PyTerm_Var of string
  | PyTerm_Lambda of string * py_term
  | PyTerm_App of py_term * py_term
  | PyTerm_Tuple of py_term * py_term
  | PyTerm_Dict of (string * py_term) list
[@@deriving show]

type py_toplevel = Bind of bool * string * py_term [@@deriving show]

type py_code = py_toplevel list [@@deriving show]

type py_term_ctx = (string * py_term) list [@@deriving show]
