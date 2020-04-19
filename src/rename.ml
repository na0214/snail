(* localで現れた束縛に対して,出現回数とdepthで修飾した名前をつける *)

open Syntax

type count_state = (int * int) list ref

let make_name name count depth =
  "__local__" ^ name ^ "_" ^ string_of_int depth ^ "_" ^ string_of_int count

let add_state depth state =
  try
    let now_count = List.assoc depth !state in
    let old_list = List.remove_assoc depth !state in
    state := (depth, now_count + 1) :: old_list
  with Not_found -> state := (depth, 0) :: !state

let rec rename name_top term depth state =
  match term with
  | Let (name, _, argument, sub_term1, sub_term2, pos) ->
      add_state depth state ;
      Let
        ( name
        , make_name name (List.assoc depth !state) depth
        , argument
        , rename name_top sub_term1 (depth + 1) state
        , rename name_top sub_term2 (depth + 1) state
        , pos )
  | App (sub_term1, sub_term2) ->
      App
        ( rename name_top sub_term1 depth state
        , rename name_top sub_term2 depth state )
  | Fun (argument, sub_term, pos) ->
      Fun (argument, rename name_top sub_term depth state, pos)
  | t ->
      t

let rename_toplevel toplevel =
  List.map
    (fun top ->
      match top with
      | LetDec (name, argument, term, pos_info) ->
          LetDec (name, argument, rename name term 0 (ref []), pos_info)
      | t ->
          t)
    toplevel
