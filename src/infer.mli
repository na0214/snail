type context = (string * Typedef.scheme) list [@@deriving show]

type subst

val get_pattern_var : Syntax.term -> string list

val get_pattern_var_unique : Syntax.term -> string list

val quantification :
  Typedef.snail_type -> ('a * Typedef.scheme) list -> Typedef.scheme

val typeof :
  bool -> string -> Syntax.term -> context -> context * Typedef.scheme

val typeof_toplevel : Syntax.snail_AST -> context -> context
