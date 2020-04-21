type context = (string * Typedef.scheme) list [@@deriving show]

type subst

val typeof :
  bool -> string -> Syntax.term -> context -> context * Typedef.snail_type

val typeof_toplevel : Syntax.snail_AST -> context -> context
