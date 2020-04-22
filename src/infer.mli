type context = (string * Typedef.scheme) list [@@deriving show]

type subst

val quantification :
  Typedef.snail_type -> ('a * Typedef.scheme) list -> Typedef.scheme

val typeof :
  bool -> string -> Syntax.term -> context -> context * Typedef.scheme

val typeof_toplevel : Syntax.snail_AST -> context -> context
