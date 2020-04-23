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

let rec print_type typ =
  match typ with
  | TyVar (Tyvar s) ->
      s
  | TyCons (Tycon s) ->
      s
  | TyApp (TyCons (Tycon s), TyApp (a, b)) when s = "->" ->
      "(" ^ print_type (TyApp (a, b)) ^ ") " ^ "->"
  | TyApp (TyCons (Tycon s), t) when s = "->" ->
      print_type t ^ " -> "
  | TyApp (TyCons (Tycon s), TyApp (a, b)) when s = "*" ->
      "(" ^ print_type (TyApp (a, b)) ^ ") " ^ "*"
  | TyApp (TyCons (Tycon s), t) when s = "*" ->
      print_type t ^ " * "
  | TyApp (t1, t2) ->
      print_type t1 ^ " " ^ print_type t2
  | TyGen n ->
      string_of_int n
  | TyPair (t1, t2) ->
      "(" ^ print_type t1 ^ "," ^ print_type t2 ^ ")"

let print_scheme sc = match sc with Forall t -> print_type t

exception TypeError of string

let arrow_t = TyCons (Tycon "->")

let product_t = TyCons (Tycon "*")

let ( @*@ ) t1 t2 = TyApp (TyApp (product_t, t1), t2)

let ( @-> ) t1 t2 = TyApp (TyApp (arrow_t, t1), t2)

type local_let_context = (string * scheme) list ref [@@deriving show]
