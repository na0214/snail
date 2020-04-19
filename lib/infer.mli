type context

type subst

val empty_context : context

val typeof : Syntax.term -> context -> Typedef.snail_type
