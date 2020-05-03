(* localで現れた束縛に対して,出現回数とdepthで修飾した名前をつける *)

open Syntax

type count_state = int ref

type rename_context = (string * string) list [@@deriving show]

exception RenameError of string * pos_info

let make_name name count = "_local__" ^ name ^ "_" ^ string_of_int count

let add_state state = state := !state + 1

let find_rename_context name ctx =
  try List.assoc name ctx with Not_found -> name

let rec get_vars_name_pattern pat =
  match pat with
  | Var (name, _, _) ->
      [name]
  | App (pat1, pat2) ->
      get_vars_name_pattern pat1 @ get_vars_name_pattern pat2
  | Prod (pat1, pat2, _) ->
      get_vars_name_pattern pat1 @ get_vars_name_pattern pat2
  | Cons (_, t_opt, _) -> (
    match t_opt with Some x -> get_vars_name_pattern x | None -> [] )
  | _ ->
      []

let generate_unique_name_list name_list state =
  List.fold_left
    (fun acc x ->
      add_state state ;
      (x, "m" ^ make_name x !state) :: acc)
    [] name_list

let rec rename_patterns pat_list state ctx =
  List.map
    (fun pat ->
      let unique_name_list =
        generate_unique_name_list (get_vars_name_pattern (fst pat)) state
      in
      let new_ctx = unique_name_list @ ctx in
      (rename (fst pat) state new_ctx, rename (snd pat) state new_ctx))
    pat_list

and rename term state ctx =
  match term with
  | Match (sub_term, pat_list, pos) ->
      Match (rename sub_term state ctx, rename_patterns pat_list state ctx, pos)
  | Let (rec_flag, name, _, argument, sub_term1, sub_term2, pos) ->
      add_state state ;
      let unique_name = make_name name !state in
      Let
        ( rec_flag
        , name
        , unique_name
        , argument
        , rename sub_term1 state ((name, unique_name) :: ctx)
        , rename sub_term2 state ((name, unique_name) :: ctx)
        , pos )
  | App (sub_term1, sub_term2) ->
      App (rename sub_term1 state ctx, rename sub_term2 state ctx)
  | Fun ([argument], _, sub_term, pos) ->
      add_state state ;
      let unique_name = "f" ^ make_name (fst argument) !state in
      Fun
        ( [argument]
        , unique_name
        , rename sub_term state ((fst argument, unique_name) :: ctx)
        , pos )
  | Prod (sub_term1, sub_term2, pos) ->
      Prod (rename sub_term1 state ctx, rename sub_term2 state ctx, pos)
  | Var (name, _, pos) ->
      Var (name, find_rename_context name ctx, pos)
  | Cons (n, sub_term, pos) as t -> (
    match sub_term with
    | Some x ->
        Cons (n, Some (rename x state ctx), pos)
    | None ->
        t )
  | TypeAnnot (sub_term, typ) ->
      TypeAnnot (rename sub_term state ctx, typ)
  | t ->
      t

let rename_toplevel toplevel =
  let count = ref 0 in
  List.map
    (fun top ->
      match top with
      | LetDec (rec_f, name, argument, term, pos_info) ->
          LetDec (rec_f, name, argument, rename term count [], pos_info)
      | t ->
          t)
    toplevel
