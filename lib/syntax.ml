open Lexing

type pos_info = {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
[@@deriving show]

type term =
  | Let of string * term * term * pos_info
  | Fun of string * term * pos_info
  | App of term * term
  | IntLit of int * pos_info
  | FloatLit of float * pos_info
  | StringLit of string * pos_info
  | Var of string * pos_info
  | Cons of string * pos_info
[@@deriving show]

exception SyntaxError of string

let translate_lexbuf_to_pos_info (pos : Lexing.position) : pos_info =
  { pos_fname= pos.pos_fname
  ; pos_lnum= pos.pos_lnum
  ; pos_bol= pos.pos_bol
  ; pos_cnum= pos.pos_cnum }

let rec get_pos_info_term t =
  match t with
  | Let (_, _, _, p) ->
      p
  | Fun (_, _, p) ->
      p
  | App (t1, _) ->
      get_pos_info_term t1
  | IntLit (_, p) ->
      p
  | FloatLit (_, p) ->
      p
  | StringLit (_, p) ->
      p
  | Var (_, p) ->
      p
  | Cons (_, p) ->
      p
