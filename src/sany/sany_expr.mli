(** Provides a conversion from the xml datastructures which directly mirror
    the sany datastructures to the internal ones defined in Expr_ds.mli
*)

type builtin_store = (int * Expr_ds.builtin_op) list
type lambda_store = (int * Expr_ds.user_defined_op_) list

(*val convert_expr :
  ?builtins:builtin_store -> Sany_ds.expr    ->   Expr_ds.expr
*)
val convert_context :
  ?builtins:builtin_store -> Sany_ds.context -> Expr_ds.context
(*
val convert_module :
  ?builtins:builtin_store -> Sany_ds.mule    ->  Expr_ds.mule
*)
val convert_formal_param :
  ?builtins:builtin_store -> Sany_ds.entry list -> Sany_ds.formal_param
  -> Expr_ds.formal_param
