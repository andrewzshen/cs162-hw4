open Ast

type env = (string * ty) list

val abstract_eval : env -> Ast.expr -> ty

val size : ty -> int option

exception Type_error of string
