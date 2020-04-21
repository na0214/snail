(* localで現れた束縛に対して,出現回数とdepthで修飾した名前をつける *)

open Syntax

type count_state = (int * int) list ref

type rename_context = (string * string) list [@@deriving show]

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

let rec get_vars_name_pattern pat =
  match pat with
  | PatternVar (name, _, _) ->
      [name]
  | PatternApp (pat1, pat2) ->
      get_vars_name_pattern pat1 @ get_vars_name_pattern pat2
  | PatternProd (pat1, pat2, _) ->
      get_vars_name_pattern pat1 @ get_vars_name_pattern pat2
  | _ ->
      []

let generate_unique_name_list name_list depth state =
  List.fold_left
    (fun acc x ->
      add_state depth state ;
      (x, make_name x (List.assoc depth !state) depth) :: acc)
    [] name_list

let rec rename_pattern pat ctx =
  match pat with
  | PatternApp (pat1, pat2) ->
      PatternApp (rename_pattern pat1 ctx, rename_pattern pat2 ctx)
  | PatternProd (pat1, pat2, pos) ->
      PatternProd (rename_pattern pat1 ctx, rename_pattern pat2 ctx, pos)
  | PatternVar (name, _, pos) ->
      PatternVar (name, find_rename_context name ctx, pos)
  | p ->
      p

let rec rename_patterns pat_list depth state ctx =
  List.map
    (fun pat ->
      let unique_name_list =
        generate_unique_name_list (get_vars_name_pattern (fst pat)) depth state
      in
      let new_ctx = unique_name_list @ ctx in
      (rename_pattern (fst pat) new_ctx, rename (snd pat) depth state new_ctx))
    pat_list

and rename term depth state ctx =
  match term with
  | Match (sub_term, pat_list, pos) ->
      Match
        ( rename sub_term depth state ctx
        , rename_patterns pat_list (depth + 1) state ctx
        , pos )
  | Let (rec_flag, name, _, argument, sub_term1, sub_term2, pos) ->
      add_state depth state ;
      let unique_name = make_name name (List.assoc depth !state) depth in
      Let
        ( rec_flag
        , name
        , unique_name
        , argument
        , rename sub_term1 (depth + 1) state ((name, unique_name) :: ctx)
        , rename sub_term2 (depth + 1) state ((name, unique_name) :: ctx)
        , pos )
  | App (sub_term1, sub_term2) ->
      App (rename sub_term1 depth state ctx, rename sub_term2 depth state ctx)
  | Fun ([argument], _, sub_term, pos) ->
      add_state depth state ;
      let unique_name = make_name argument (List.assoc depth !state) depth in
      Fun
        ( [argument]
        , unique_name
        , rename sub_term (depth + 1) state ((argument, unique_name) :: ctx)
        , pos )
  | Prod (sub_term1, sub_term2, pos) ->
      Prod
        (rename sub_term1 depth state ctx, rename sub_term2 depth state ctx, pos)
  | Var (name, _, pos) ->
      Var (name, find_rename_context name ctx, pos)
  | t ->
      t

let rename_toplevel toplevel =
  List.map
    (fun top ->
      match top with
      | LetDec (rec_f, name, argument, term, pos_info) ->
          LetDec (rec_f, name, argument, rename term 0 (ref []) [], pos_info)
      | t ->
          t)
    toplevel
