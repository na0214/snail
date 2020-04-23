open Syntax
open Typedef

type subst = (tyvar * snail_type) list

type context = (string * scheme) list [@@deriving show]

let rec get_unique_tyvar typ =
  match typ with
  | TyVar (Tyvar n) ->
      [Tyvar n]
  | TyApp (typ1, typ2) ->
      ExtList.List.unique (get_unique_tyvar typ1 @ get_unique_tyvar typ2)
  | TyPair (typ1, typ2) ->
      ExtList.List.unique (get_unique_tyvar typ1 @ get_unique_tyvar typ2)
  | _ ->
      []

let diff_list l1 l2 = List.filter (fun x -> List.mem x l2 |> not) l1

let rec unfold_right f init =
  match f init with None -> [] | Some (x, next) -> x :: unfold_right f next

let range n =
  let irange x = if x > n then None else Some (x, x + 1) in
  unfold_right irange 1

let rec apply_subst sb typ =
  match typ with
  | TyVar v -> (
    try List.assoc v sb with Not_found -> TyVar v )
  | TyApp (typ1, typ2) ->
      TyApp (apply_subst sb typ1, apply_subst sb typ2)
  | TyPair (typ1, typ2) ->
      TyPair (apply_subst sb typ1, apply_subst sb typ2)
  | other_typ ->
      other_typ

let quantification typ ctx =
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
        TypeError "not match length" |> raise
  in
  Forall (apply_subst tyvar_with_gen typ)

let find_context name ctx =
  try List.assoc name ctx
  with Not_found -> TypeError ("unbound identifier: " ^ name) |> raise

let append_subst sb1 sb2 =
  sb1 @ List.map (fun x -> match x with u, t -> (u, apply_subst sb1 t)) sb2

let var_bind v t =
  if t = TyVar v then []
  else if List.mem v (get_unique_tyvar t) then
    TypeError "occurs check fails" |> raise
  else [(v, t)]

let rec mgu typ1 typ2 =
  match (typ1, typ2) with
  | TyApp (tl1, tr1), TyApp (tl2, tr2) ->
      let s1 = mgu tl1 tl2 in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 tr2) in
      append_subst s2 s1
  | TyPair (tl1, tr1), TyApp (TyApp (TyCons (Tycon "*"), t1), t2) ->
      let s1 = mgu tl1 t1 in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 t2) in
      append_subst s2 s1
  | TyPair (tl1, tr1), TyPair (tl2, tr2) ->
      let s1 = mgu tl1 tl2 in
      let s2 = mgu (apply_subst s1 tr1) (apply_subst s1 tr2) in
      append_subst s2 s1
  | TyVar v, t ->
      var_bind v t
  | t, TyVar v ->
      var_bind v t
  | TyCons tc1, TyCons tc2 when tc1 = tc2 ->
      []
  | _ ->
      print_string
        (Typedef.print_type typ1 ^ " : " ^ Typedef.print_type typ2 ^ "\n") ;
      TypeError "types do not unify" |> raise

let unify typ1 typ2 sb =
  match !sb with
  | s, n ->
      let u = mgu (apply_subst s typ1) (apply_subst s typ2) in
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
  | TyGen n ->
      List.assoc n tyvar_list
  | t ->
      t

let fresh_inst sc sb =
  match sc with
  | Forall t ->
      let new_tyvar_list =
        List.fold_left
          (fun acc n -> (n, new_tyvar sb) :: acc)
          []
          (List.length (get_subst sb) + 1 |> range)
      in
      instantiate t new_tyvar_list

let add_local_let_context local name typ = local := (name, typ) :: !local

let rec get_pattern_var term =
  match term with
  | Prod (t1, t2, _) ->
      get_pattern_var t1 @ get_pattern_var t2
  | App (t1, t2) ->
      get_pattern_var t1 @ get_pattern_var t2
  | Var (n, _, _) ->
      [n]
  | _ ->
      []

let generate_pattern_var_ctx sb bind_var =
  List.map (fun v -> (v, Forall (new_tyvar sb))) bind_var

let rec infer term typ (ctx : context) sb (local : local_let_context) =
  match term with
  | IntLit (_, _) ->
      unify (TyCons (Tycon "Int")) typ sb
  | FloatLit (_, _) ->
      unify (TyCons (Tycon "Float")) typ sb
  | StringLit (_, _) ->
      unify (TyCons (Tycon "String")) typ sb
  | Fun ([name], _, sub_term, _) ->
      let a = new_tyvar sb in
      let b = new_tyvar sb in
      unify (a @-> b) typ sb ;
      let new_ctx = (name, Forall a) :: ctx in
      infer sub_term b new_ctx sb local
  | Var (name, _, _) ->
      let sc = find_context name ctx in
      let typ1 = fresh_inst sc sb in
      unify typ1 typ sb
  | App (sub_term1, sub_term2) ->
      let a = new_tyvar sb in
      infer sub_term1 (a @-> typ) ctx sb local ;
      infer sub_term2 a ctx sb local ;
      print_string
        ( "["
        ^ Typedef.print_scheme
            (quantification (apply_subst (get_subst sb) (a @-> typ)) ctx)
        ^ " :: "
        ^ Typedef.print_scheme
            (quantification (apply_subst (get_subst sb) a) ctx)
        ^ "]" ^ "\n" )
  | Let (rec_flag, name, unique_name, _, sub_term1, sub_term2, _) ->
      let a = new_tyvar sb in
      if rec_flag then
        let b = new_tyvar sb in
        infer sub_term1 a ((name, Forall b) :: ctx) sb local
      else infer sub_term1 a ctx sb local ;
      add_local_let_context local unique_name
        (quantification (apply_subst (get_subst sb) a) ctx) ;
      let new_ctx =
        (name, quantification (apply_subst (get_subst sb) a) ctx) :: ctx
      in
      infer sub_term2 typ new_ctx sb local
  | Cons (name, _) ->
      let sc = find_context name ctx in
      let typ1 = fresh_inst sc sb in
      unify typ1 typ sb
  | Prod (sub_term1, sub_term2, _) ->
      let a = new_tyvar sb in
      let b = new_tyvar sb in
      infer sub_term1 a ctx sb local ;
      infer sub_term2 b ctx sb local ;
      unify (a @*@ b) typ sb
  | Match (sub_term, pat_list, _) ->
      let sub_v = new_tyvar sb in
      infer sub_term sub_v ctx sb local ;
      List.iter
        (fun (pat, sub_t) ->
          let pat_v = new_tyvar sb in
          let bind_var_ctx =
            pat |> get_pattern_var |> generate_pattern_var_ctx sb
          in
          infer pat pat_v (bind_var_ctx @ ctx) sb local ;
          unify sub_v pat_v sb ;
          infer sub_t typ (bind_var_ctx @ ctx) sb local)
        pat_list
  | _ ->
      raise (TypeError "type error")

let typeof rec_flag name term ctx =
  let sb = ref ([], 0) in
  let local_let_ctx = ref [] in
  let result_t = new_tyvar sb in
  if rec_flag then (
    let b = new_tyvar sb in
    infer term b ((name, Forall b) :: ctx) sb local_let_ctx ;
    (!local_let_ctx, quantification (apply_subst (get_subst sb) b) []) )
  else (
    infer term result_t ctx sb local_let_ctx ;
    (!local_let_ctx, quantification (apply_subst (get_subst sb) result_t) []) )

let default_context = []

let typeof_toplevel toplevel context =
  let local_context = ref [] in
  let new_ctx =
    List.fold_left
      (fun acc x ->
        ( match x with
        | LetDec (rec_flag, name, _, sub_term, _) ->
            let typ = typeof rec_flag name sub_term acc in
            local_context := !local_context @ fst typ ;
            (name, snd typ)
        | _ ->
            ("", Forall (TyCons (Tycon "None"))) )
        :: acc)
      (context @ default_context)
      toplevel
  in
  !local_context @ new_ctx
