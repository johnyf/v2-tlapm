(**
 This file contains helper function to resolve references to expressions.
*)
open Expr_ds


(** retrieves the entry with the given id from the context and transforms it.
    the dereference function use Expr_utils.unpack_* as transformer.
*)
val find_entry : (entry -> 'a) -> context -> int -> 'a

(** If the passed operator is a reference, it will be retrieved from the
 context. Otherwise, its content is directly extracted. *)
val dereference_user_defined_op : context -> user_defined_op -> user_defined_op_

(** If the passed formal parameter is a reference, it will be retrieved from the
 context. Otherwise, its content is directly extracted. *)
val dereference_formal_param : context -> formal_param -> formal_param_

(** If the passed operator declaration is a reference, it will be retrieved from
 the context. Otherwise, it is directly extracted. *)
val dereference_op_decl : context -> op_decl -> op_decl_

(** If the passed operator definition is a reference, it will be retrieved from
 the context. Otherwise, it is directly extracted. *)
val dereference_op_def : context -> op_def -> op_def_

(** If the passed theorem is a reference, it will be retrieved from the
 context. Otherwise, it is directly extracted. *)
val dereference_theorem : context -> theorem -> theorem_
