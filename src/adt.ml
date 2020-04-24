open Syntax
open Typedef
open Infer

let generate_tyvars_product tyvars =
  match tyvars with
  | [] ->
      TyCon (Tycon "type error")
  | h :: xs ->
      List.fold_left
        (fun acc x -> TyPair (acc, TyVar (Tyvar x)))
        (TyVar (Tyvar h)) xs

let generate_adt_type type_name tyvars typ =
  if List.length tyvars = 0 then
    quantification (typ @-> TyCon (Tycon type_name)) []
  else
    quantification
      (typ @-> TyApp (TyCon (Tycon type_name), generate_tyvars_product tyvars))
      []

let generate_adt_context toplevel =
  List.fold_left
    (fun acc_1 x ->
      match x with
      | TypeDef (type_name, tyvars, value_cons, _) ->
          acc_1
          @ List.fold_left
              (fun acc_2 value_con ->
                match snd value_con with
                | Some vc ->
                    (fst value_con, generate_adt_type type_name tyvars vc)
                    :: acc_2
                | None ->
                    ( fst value_con
                    , quantification
                        (TyApp
                           ( TyCon (Tycon type_name)
                           , generate_tyvars_product tyvars ))
                        [] )
                    :: acc_2)
              [] value_cons
      | _ ->
          acc_1 @ [])
    [] toplevel
