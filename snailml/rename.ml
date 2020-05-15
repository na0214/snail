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

let make_mutual_bind_context mut_bind state =
  List.map
    (fun x ->
      match x with
      | MutLetBind (name, _, _, _, _, _) ->
          (name, make_name name !state)
      | _ ->
          ("error", "error"))
    mut_bind

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
  | Let
      ( rec_flag
      , name
      , _
      , argument
      , sub_term1
      , sub_term2
      , type_annot
      , pos
      , let_bind ) ->
      add_state state ;
      let unique_name = make_name name !state in
      let new_context =
        ((name, unique_name) :: make_mutual_bind_context let_bind state) @ ctx
      in
      Let
        ( rec_flag
        , name
        , unique_name
        , argument
        , rename sub_term1 state new_context
        , rename sub_term2 state new_context
        , type_annot
        , pos
        , List.map (fun x -> rename x state new_context) let_bind )
  | MutLetBind (name, _, arguments, sub_term, type_annot, pos) ->
      let uname = find_rename_context name ctx in
      MutLetBind
        (name, uname, arguments, rename sub_term state ctx, type_annot, pos)
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

let rec rename_toplevel toplevel =
  let count = ref 0 in
  List.map
    (fun top ->
      match top with
      | LetDec (rec_f, name, argument, term, type_annot, pos_info, let_bind) ->
          LetDec
            ( rec_f
            , name
            , argument
            , rename term count []
            , type_annot
            , pos_info
            , rename_toplevel let_bind )
      | t ->
          t)
    toplevel
