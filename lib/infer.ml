open Syntax
open Type

let rec infer term ctx =
  match term with
  | IntLit (_, _) ->
      TCons "Int"
  | FloatLit (_, _) ->
      TCons "Float"
  | StringLit (_, _) ->
      TCons "String"
  | Let (name, t1, t2, _) ->
      TCons "undefined"
  | Fun (name, t, _) ->
      TCons "undefined"
  | App (t1, t2) ->
      TCons "undefined"
  | Var (name, _) ->
      TCons "undefined"
  | Cons (name, _) ->
      TCons "undefined"
