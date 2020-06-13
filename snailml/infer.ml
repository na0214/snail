open Syntax
open Typedef

type subst = (tyvar * snail_type) list

type context = (string * scheme) list [@@deriving show]

exception TypeError of string * pos_info

let rec get_unique_tyvar = function
  | TyVar (Tyvar n) ->
      [Tyvar n]
  | TyApp (typ1, typ2) ->
      ExtList.List.unique (get_unique_tyvar typ1 @ get_unique_tyvar typ2)
  | TyPair (typ1, typ2) ->
      ExtList.List.unique (get_unique_tyvar typ1 @ get_unique_tyvar typ2)
  | TyExp (typ1, typ2) ->
      ExtList.List.unique (get_unique_tyvar typ1 @ get_unique_tyvar typ2)
  | _ ->
      []

let diff_list l1 l2 = List.filter (fun x -> List.mem x l2 |> not) l1

let rec unfold_right f init =
  match f init with None -> [] | Some (x, next) -> x :: unfold_right f next

let range n =
  let irange x = if x > n then None else Some (x, x + 1) in
  unfold_right irange 1

let rec apply_subst sb = function
  | TyVar v -> (
    try List.assoc v sb with Not_found -> TyVar v )
  | TyApp (typ1, typ2) ->
      TyApp (apply_subst sb typ1, apply_subst sb typ2)
  | TyPair (typ1, typ2) ->
      TyPair (apply_subst sb typ1, apply_subst sb typ2)
  | TyExp (typ1, typ2) ->
      TyExp (apply_subst sb typ1, apply_subst sb typ2)
  | other_typ ->
      other_typ

let quantification typ ctx pos =
  let unique_tyvar =
    diff_list (get_unique_tyvar typ)
      (List.fold_left
         (fun acc ct -> match ct with _, Forall t -> acc @ get_unique_tyvar t)
         [] ctx)
  in
  let tyvar_with_gen =
    match
      Base.List.zip unique_tyvar
        (List.map (fun x -> TyGen x) (List.length unique_tyvar |> range))
    with
    | Base.List.Or_unequal_lengths.Ok zl ->
        zl
    | Base.List.Or_unequal_lengths.Unequal_lengths ->
        TypeError ("not match length", pos) |> raise
  in
  Forall (apply_subst tyvar_with_gen typ)

let find_context name ctx pos =
  try List.assoc name ctx
  with Not_found -> TypeError ("unbound identifier: " ^ name, pos) |> raise

let append_subst sb1 sb2 =
  sb1 @ List.map (function u, t -> (u, apply_subst sb1 t)) sb2

let var_bind v t pos =
  if t = TyVar v then []
  else if List.mem v (get_unique_tyvar t) then
    TypeError ("occurs check fails", pos) |> raise
  else [(v, t)]

let rec mgu typ1 typ2 pos =
  match (typ1, typ2) with
  | TyApp (tl1, tr1), TyApp (tl2, tr2) ->
      let s1 = mgu tl1 tl2 pos in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 tr2) pos in
      append_subst s2 s1
  | TyPair (tl1, tr1), TyApp (TyApp (TyCon (Tycon "*"), t1), t2) ->
      let s1 = mgu tl1 t1 pos in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 t2) pos in
      append_subst s2 s1
  | TyPair (tl1, tr1), TyPair (tl2, tr2) ->
      let s1 = mgu tl1 tl2 pos in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 tr2) pos in
      append_subst s2 s1
  | TyVar v, t ->
      var_bind v t pos
  | t, TyVar v ->
      var_bind v t pos
  | TyCon tc1, TyCon tc2 when tc1 = tc2 ->
      []
  | Infinity, Infinity ->
      []
  | TyExp (n1, t1), TyExp (n2, t2) ->
      let s1 = mgu n1 n2 pos in
      let s2 = mgu (apply_subst s1 t1) (apply_subst s1 t2) pos in
      append_subst s2 s1
  | Int n1, Int n2 ->
      if n1 <= n2 then []
      else
        TypeError
          (string_of_int n1 ^ " <= " ^ string_of_int n2 ^ " is invalid ", pos)
        |> raise
  | Int _, Infinity ->
      []
  | _ ->
      print_string
        (Typedef.print_type typ1 ^ " : " ^ Typedef.print_type typ2 ^ "\n") ;
      TypeError ("types do not unify", pos) |> raise

let unify typ1 typ2 sb pos =
  match !sb with
  | s, n ->
      let u = mgu (apply_subst s typ1) (apply_subst s typ2) pos in
      sb := (append_subst u s, n)

let new_tyvar sb =
  match !sb with
  | s, n ->
      sb := (s, n + 1) ;
      TyVar (Tyvar (string_of_int n))

let get_subst sb = match !sb with s, _ -> s

let rec instantiate typ tyvar_list =
  match typ with
  | TyApp (typ1, typ2) ->
      TyApp (instantiate typ1 tyvar_list, instantiate typ2 tyvar_list)
  | TyPair (typ1, typ2) ->
      TyPair (instantiate typ1 tyvar_list, instantiate typ2 tyvar_list)
  | TyExp (typ1, typ2) ->
      TyExp (instantiate typ1 tyvar_list, instantiate typ2 tyvar_list)
  | TyGen n ->
      List.assoc n tyvar_list
  | t ->
      t

let rec get_max_tygen = function
  | TyApp (typ1, typ2) | TyPair (typ1, typ2) | TyExp (typ1, typ2) ->
      max (get_max_tygen typ1) (get_max_tygen typ2)
  | TyGen n ->
      n
  | _ ->
      0

let fresh_inst sc sb =
  match sc with
  | Forall t ->
      let new_tyvar_list =
        List.fold_left
          (fun acc n -> (n, new_tyvar sb) :: acc)
          []
          (get_max_tygen t |> range)
      in
      instantiate t new_tyvar_list

let add_local_let_context local name typ = local := (name, typ) :: !local

let rec get_pattern_var = function
  | Prod (t1, t2, _) ->
      get_pattern_var t1 @ get_pattern_var t2
  | App (t1, t2) ->
      get_pattern_var t1 @ get_pattern_var t2
  | Var (n, _, _) ->
      [n]
  | Cons (_, opt_app, _) -> (
    match opt_app with Some x -> get_pattern_var x | None -> [] )
  | _ ->
      []

let rec get_pattern_var_unique = function
  | Prod (t1, t2, _) ->
      get_pattern_var_unique t1 @ get_pattern_var_unique t2
  | App (t1, t2) ->
      get_pattern_var_unique t1 @ get_pattern_var_unique t2
  | Var (_, n, _) ->
      [n]
  | Cons (_, opt_app, _) -> (
    match opt_app with Some x -> get_pattern_var_unique x | None -> [] )
  | _ ->
      []

let generate_pattern_var_ctx sb bind_var =
  List.map (fun v -> (v, Forall (new_tyvar sb))) bind_var

let generate_mutual_bind_context_let let_bind sb =
  List.map
    (fun x ->
      match x with
      | MutLetBind (name, _, _, _, _, _) ->
          (name, Forall (new_tyvar sb))
      | _ ->
          ("error", Forall (TyCon (Tycon "error"))))
    let_bind

let add_coeff coeff1 coeff2 =
  match (coeff1, coeff2) with Int n1, Int n2 -> Int (n1 + n2) | _ -> Infinity

let add_coeff_ctx coeff1 coeff2 =
  let unique_coeffect =
    List.map (fun (a, _) -> a) (coeff1 @ coeff2 |> ExtList.List.unique)
  in
  List.map
    (fun coeff ->
      let coeff_from_coeff1 =
        try List.assoc coeff coeff1 with Not_found -> Int 0
      in
      let coeff_from_coeff2 =
        try List.assoc coeff coeff2 with Not_found -> Int 0
      in
      (coeff, add_coeff coeff_from_coeff1 coeff_from_coeff2))
    unique_coeffect

let leq_coeff coeff1 coeff2 =
  match (coeff1, coeff2) with
  | Int n1, Int n2 ->
      n1 <= n2
  | Infinity, Int _ ->
      false
  | Int _, Infinity ->
      true
  | _ ->
      true

let rec infer term typ (ctx : context) sb (local : local_let_context) =
  match term with
  | IntLit (_, pos) ->
      unify (TyCon (Tycon "Int")) typ sb pos ;
      []
  | FloatLit (_, pos) ->
      unify (TyCon (Tycon "Float")) typ sb pos ;
      []
  | StringLit (_, pos) ->
      unify (TyCon (Tycon "String")) typ sb pos ;
      []
  | Fun ([(name, Some (Forall (TyExp (sring, t) as t_annot)))], _, sub_term, pos)
    ->
      let a = new_tyvar sb in
      let b = new_tyvar sb in
      unify a t_annot sb pos ;
      unify (a @-> b) typ sb pos ;
      let new_ctx = (name, Forall t) :: ctx in
      let coeff_term = infer sub_term b new_ctx sb local in
      let coeff = try List.assoc name coeff_term with Not_found -> Int 0 in
      if leq_coeff coeff sring then coeff_term
      else raise (TypeError ("usage error", pos))
  | Fun ([(name, type_annot)], _, sub_term, pos) ->
      let a = new_tyvar sb in
      let b = new_tyvar sb in
      ( match type_annot with
      | Some (Forall t_annot) ->
          unify a t_annot sb pos
      | None ->
          () ) ;
      unify (a @-> b) typ sb pos ;
      let new_ctx = (name, Forall a) :: ctx in
      infer sub_term b new_ctx sb local
  | Var (name, _, pos) ->
      let sc = find_context name ctx pos in
      let typ1 = fresh_inst sc sb in
      unify typ1 typ sb pos ; [(name, Int 1)]
  | TypeAnnot (sub_term, Forall typ11) ->
      let annot_pos = Syntax.get_pos_info_term sub_term in
      let coeff = infer sub_term typ ctx sb local in
      unify typ typ11 sb annot_pos ;
      coeff
  | App (sub_term1, sub_term2) ->
      let a = new_tyvar sb in
      add_coeff_ctx
        (infer sub_term1 (a @-> typ) ctx sb local)
        (infer sub_term2 a ctx sb local)
  | Let (rec_flag, name, unique_name, _, sub_term1, sub_term2, _, pos, []) ->
      let a = new_tyvar sb in
      let sub_term1_coeff =
        if rec_flag then
          let b = new_tyvar sb in
          infer sub_term1 a ((name, Forall b) :: ctx) sb local
        else infer sub_term1 a ctx sb local
      in
      add_local_let_context local unique_name
        (quantification (apply_subst (get_subst sb) a) ctx pos) ;
      let new_ctx =
        (name, quantification (apply_subst (get_subst sb) a) ctx pos) :: ctx
      in
      add_coeff_ctx sub_term1_coeff (infer sub_term2 typ new_ctx sb local)
  | Let (_, name, unique_name, _, sub_term1, sub_term2, _, pos, let_bind) ->
      let a = new_tyvar sb in
      let new_ctx =
        ((name, Forall a) :: generate_mutual_bind_context_let let_bind sb) @ ctx
      in
      let _ = infer sub_term1 a new_ctx sb local in
      add_local_let_context local unique_name
        (quantification (apply_subst (get_subst sb) a) ctx pos) ;
      List.iter
        (function
          | MutLetBind (name, unique_name, _, sub_term, _, pos) ->
              let tyvar =
                match find_context name new_ctx pos with Forall x -> x
              in
              let _ = infer sub_term tyvar new_ctx sb local in
              add_local_let_context local unique_name
                (quantification (apply_subst (get_subst sb) tyvar) new_ctx pos)
          | _ ->
              print_string "error")
        let_bind ;
      infer sub_term2 typ new_ctx sb local
  | Cons (name, app_term, pos) -> (
      let sc = find_context name ctx pos in
      let typ1 = fresh_inst sc sb in
      match app_term with
      | Some app ->
          let a = new_tyvar sb in
          let coeff_ctx = infer app a ctx sb local in
          unify typ1 (a @-> typ) sb pos ;
          coeff_ctx
      | None ->
          unify typ1 typ sb pos ; [] )
  | Prod (sub_term1, sub_term2, pos) ->
      let a = new_tyvar sb in
      let b = new_tyvar sb in
      let sub_term1_coeff = infer sub_term1 a ctx sb local in
      let sub_term2_coeff = infer sub_term2 b ctx sb local in
      unify (a @*@ b) typ sb pos ;
      add_coeff_ctx sub_term1_coeff sub_term2_coeff
  | Match (sub_term, pat_list, _) ->
      let sub_v = new_tyvar sb in
      let sub_term_coeff = infer sub_term sub_v ctx sb local in
      let pat_coeff =
        List.fold_left
          (fun acc (pat, sub_t) ->
            let pat_v = new_tyvar sb in
            let bind_var_ctx =
              pat |> get_pattern_var |> generate_pattern_var_ctx sb
            in
            let _ = infer pat pat_v (bind_var_ctx @ ctx) sb local in
            unify sub_v pat_v sb (Syntax.get_pos_info_term sub_t) ;
            acc @ infer sub_t typ (bind_var_ctx @ ctx) sb local)
          [] pat_list
      in
      add_coeff_ctx sub_term_coeff pat_coeff
  | ExpMod sub_term ->
      let a = new_tyvar sb in
      let coeff = infer sub_term a ctx sb local in
      unify (TyExp (new_tyvar sb, a)) typ sb (Syntax.get_pos_info_term sub_term) ;
      coeff
  | _ ->
      []

let generate_mutual_bind_context_top let_bind sb =
  List.map
    (fun x ->
      match x with
      | LetDec (_, name, _, _, _, _, _) ->
          (name, Forall (new_tyvar sb))
      | _ ->
          ("error", Forall (TyCon (Tycon "error"))))
    let_bind

let typeof rec_flag name term ctx =
  let sb = ref ([], 0) in
  let local_let_ctx = ref [] in
  let result_t = new_tyvar sb in
  if rec_flag then
    let b = new_tyvar sb in
    let _ = infer term b ((name, Forall b) :: ctx) sb local_let_ctx in
    ( !local_let_ctx
    , quantification
        (apply_subst (get_subst sb) b)
        []
        (Syntax.get_pos_info_term term) )
  else
    let _ = infer term result_t ctx sb local_let_ctx in
    ( !local_let_ctx
    , quantification
        (apply_subst (get_subst sb) result_t)
        []
        (Syntax.get_pos_info_term term) )

let typeof_mutually_recursive name term let_bind ctx local_let_ctx =
  let sb = ref ([], 0) in
  let a = new_tyvar sb in
  let new_tyvar_ctx =
    (name, Forall a) :: generate_mutual_bind_context_top let_bind sb
  in
  let new_ctx = new_tyvar_ctx @ ctx in
  let _ = infer term a new_ctx sb local_let_ctx in
  List.iter
    (fun x ->
      match x with
      | LetDec (_, name, _, sub_term, _, _, _) ->
          let _ =
            infer sub_term
              ( match find_context name new_ctx (get_pos_info_term sub_term) with
              | Forall x ->
                  x )
              new_ctx sb local_let_ctx
          in
          ()
      | _ ->
          print_string "error")
    let_bind ;
  List.map
    (fun (n, Forall v) ->
      ( n
      , quantification
          (apply_subst (get_subst sb) v)
          []
          (Syntax.get_pos_info_term term) ))
    new_tyvar_ctx

let typeof_toplevel toplevel context =
  let local_context = ref [] in
  let new_ctx =
    List.fold_left
      (fun acc x ->
        ( match x with
        | LetDec (rec_flag, name, _, sub_term, _, _, []) ->
            let typ = typeof rec_flag name sub_term acc in
            local_context := !local_context @ fst typ ;
            [(name, snd typ)]
        | LetDec (_, name, _, sub_term, _, _, let_bind) ->
            let local_let_ctx = ref [] in
            let type_ctx =
              typeof_mutually_recursive name sub_term let_bind acc local_let_ctx
            in
            local_context := !local_context @ !local_let_ctx ;
            type_ctx
        | _ ->
            [("__none__", Forall (TyCon (Tycon "None")))] )
        @ acc)
      context toplevel
  in
  !local_context @ new_ctx
