type context = (string * Typedef.scheme) list [@@deriving show]

type subst

val empty_context : context

val typeof : Syntax.term -> context -> Typedef.snail_type

val typeof_toplevel : Syntax.snail_AST -> context -> context
