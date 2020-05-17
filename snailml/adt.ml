open Syntax
open Typedef
open Infer

(* generate type-level products from multiple type variables *)
(* ex. translate (List a b) to (List (a,b)) *)
let generate_tyvars_product = function
  | [] ->
      TyCon (Tycon "type error")
  | h :: xs ->
      List.fold_left
        (fun acc x -> TyPair (acc, TyVar (Tyvar x)))
        (TyVar (Tyvar h)) xs

(* generate value-constructor's type *)
let generate_adt_type type_name tyvars typ pos =
  if List.length tyvars = 0 then
    quantification (typ @-> TyCon (Tycon type_name)) [] pos
  else
    quantification
      (typ @-> TyApp (TyCon (Tycon type_name), generate_tyvars_product tyvars))
      [] pos

let generate_constructor value_con type_name tyvars pos =
  match snd value_con with
  | Some vc ->
      (fst value_con, generate_adt_type type_name tyvars vc pos)
  | None ->
      ( fst value_con
      , quantification
          (TyApp (TyCon (Tycon type_name), generate_tyvars_product tyvars))
          [] pos )

(* generate value-constructor's contexts from ADT definitions *)
let rec generate_adt_context toplevel =
  List.fold_left
    (fun acc_1 x ->
      match x with
      | TypeDef (type_name, tyvars, value_cons, pos, typedef) ->
          acc_1
          @ List.fold_left
              (fun acc_2 value_con ->
                generate_constructor value_con type_name tyvars pos :: acc_2)
              [] value_cons
          @ generate_adt_context typedef
      | _ ->
          acc_1)
    [] toplevel
