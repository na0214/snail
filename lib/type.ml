type snail_type =
  | TCons of string
  | TArrow of snail_type * snail_type
  | TVar of string
  | Forall of string
[@@deriving show]
