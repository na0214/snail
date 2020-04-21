open Lexing

type pos_info = {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
[@@deriving show]

type argument = string [@@deriving show]

type rec_flag = bool [@@deriving show]

type term =
  | Let of rec_flag * string * string * argument list * term * term * pos_info
  | Fun of argument list * string * term * pos_info
  | App of term * term
  | IntLit of int * pos_info
  | FloatLit of float * pos_info
  | StringLit of string * pos_info
  | Var of string * string * pos_info
  | Cons of string * pos_info
[@@deriving show]

exception SyntaxError of string

type toplevel = LetDec of string * argument list * term * pos_info | Empty
[@@deriving show]

type snail_AST = toplevel list [@@deriving show]

let translate_lexbuf_to_pos_info (pos : Lexing.position) : pos_info =
  { pos_fname= pos.pos_fname
  ; pos_lnum= pos.pos_lnum
  ; pos_bol= pos.pos_bol
  ; pos_cnum= pos.pos_cnum }

let rec get_pos_info_term t =
  match t with
  | Let (_, _, _, _, _, _, p) ->
      p
  | Fun (_, _, _, p) ->
      p
  | App (t1, _) ->
      get_pos_info_term t1
  | IntLit (_, p) ->
      p
  | FloatLit (_, p) ->
      p
  | StringLit (_, p) ->
      p
  | Var (_, _, p) ->
      p
  | Cons (_, p) ->
      p
