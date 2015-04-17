(** Provides a conversion from the xml datastructures which directly mirror
    the sany datastructures to the internal ones defined in Expr_ds.mli
 *)

val convert_expr    : Sany_ds.expr    -> Expr_ds.expr
val convert_context : Sany_ds.context -> Expr_ds.context
val convert_module  : Sany_ds.mule    -> Expr_ds.mule
