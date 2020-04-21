type tycons = Tycon of string [@@deriving show]

type tyvar = Tyvar of string [@@deriving show]

type snail_type =
  | TyCons of tycons
  | TyVar of tyvar
  | TyApp of snail_type * snail_type
  | TyGen of int
  | TyPair of snail_type * snail_type
[@@deriving show]

type scheme = Forall of snail_type [@@deriving show]

exception TypeError of string

let arrow_t = TyCons (Tycon "->")

let product_t = TyCons (Tycon "*")

let ( @*@ ) t1 t2 = TyApp (TyApp (product_t, t1), t2)

let ( @-> ) t1 t2 = TyApp (TyApp (arrow_t, t1), t2)

type local_let_context = (string * scheme) list ref [@@deriving show]
