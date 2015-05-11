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
}

(** creates a test result from the filename and optionally the contexts. *)
val mkTestResult :
  ?sc:Sany_ds.context -> ?ec:Expr_ds.context -> string -> test_result


(** Evaluates the given function and pretty prints an exception,
    if it is raised. In that case, the exception is raised again for
    the caller to catch too.
 *)
val exhandler : (unit -> 'a) -> 'a
