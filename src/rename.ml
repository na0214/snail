(* localで現れた束縛に対して,出現回数とdepthで修飾した名前をつける *)

open Syntax

type count_state = (int * int) list ref

type rename_context = (string * string) list

exception RenameError of string

let make_name name count depth =
  "__local__" ^ name ^ "_" ^ string_of_int depth ^ "_" ^ string_of_int count

let add_state depth state =
  try
    let now_count = List.assoc depth !state in
    let old_list = List.remove_assoc depth !state in
    state := (depth, now_count + 1) :: old_list
  with Not_found -> state := (depth, 0) :: !state

let find_rename_context name ctx =
  try List.assoc name ctx with Not_found -> name

let rec rename name_top term depth state ctx =
  match term with
  | Let (rec_flag, name, _, argument, sub_term1, sub_term2, pos) ->
      add_state depth state ;
      let unique_name = make_name name (List.assoc depth !state) depth in
      Let
        ( rec_flag
        , name
        , unique_name
        , argument
        , rename name_top sub_term1 (depth + 1) state
            ((name, unique_name) :: ctx)
        , rename name_top sub_term2 (depth + 1) state
            ((name, unique_name) :: ctx)
        , pos )
  | App (sub_term1, sub_term2) ->
      App
        ( rename name_top sub_term1 depth state ctx
        , rename name_top sub_term2 depth state ctx )
  | Fun ([argument], _, sub_term, pos) ->
      add_state depth state ;
      let unique_name = make_name argument (List.assoc depth !state) depth in
      Fun
        ( [argument]
        , unique_name
        , rename name_top sub_term (depth + 1) state
            ((argument, unique_name) :: ctx)
        , pos )
  | Prod (sub_term1, sub_term2, pos) ->
      Prod
        ( rename name_top sub_term1 depth state ctx
        , rename name_top sub_term2 depth state ctx
        , pos )
  | Var (name, _, pos) ->
      Var (name, find_rename_context name ctx, pos)
  | t ->
      t

let rename_toplevel toplevel =
  List.map
    (fun top ->
      match top with
      | LetDec (name, argument, term, pos_info) ->
          LetDec (name, argument, rename name term 0 (ref []) [], pos_info)
      | t ->
          t)
    toplevel
