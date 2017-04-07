open Sany_ds
open Expr_ds

(** A record storing a test result. The tests update the record
    with their results in the tear down phase, because they need
    to return the unit type.
*)
type test_result = {
  filename : string;
  mutable sany_context : Sany_ds.context option;
  mutable expr_context : Expr_ds.context option;
  mutable explicit_lambda_context : Expr_ds.context option;
  mutable explicit_steps_context : Expr_ds.context option;
  mutable obligations  : Obligation.obligation list;
  mutable simple_obligations : Simple_expr_ds.tla_simple_pb list;
}

(** creates a test result from the filename and optionally the contexts. *)
val mkTestResult :
  ?sc:Sany_ds.context -> ?ec:Expr_ds.context -> ?lc:Expr_ds.context
  -> ?esc:Expr_ds.context
  -> ?ob:Obligation.obligation list
  -> ?sob:Simple_expr_ds.tla_simple_pb list
  -> string -> test_result


(** Evaluates the given function and pretty prints an exception,
    if it is raised. In that case, the exception is raised again for
    the caller to catch too.
*)
val exhandler : (unit -> 'a) -> 'a
