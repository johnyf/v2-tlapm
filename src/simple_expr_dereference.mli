(**
 This file contains helper function to resolve references to expressions.
*)
open Simple_expr_ds


(** retrieves the entry with the given id from the term_db and transforms it.
    the dereference function use Expr_utils.unpack_* as transformer.
*)
val find_entry : (simple_entry -> 'a) -> simple_term_db -> int -> 'a

(** If the passed operator is a reference, it will be retrieved from the
 term_db. Otherwise, its content is directly extracted. *)
val dereference_user_defined_op : simple_term_db -> simple_user_defined_op -> simple_user_defined_op_

(** If the passed formal parameter is a reference, it will be retrieved from the
 term_db. Otherwise, its content is directly extracted. *)
val dereference_formal_param : simple_term_db -> simple_formal_param -> simple_formal_param_

(** If the passed operator declaration is a reference, it will be retrieved from
 the term_db. Otherwise, it is directly extracted. *)
val dereference_op_decl : simple_term_db -> simple_op_decl -> simple_op_decl_

(** If the passed operator definition is a reference, it will be retrieved from
 the term_db. Otherwise, it is directly extracted. *)
val dereference_user_defined_op : simple_term_db -> simple_user_defined_op -> simple_user_defined_op_
