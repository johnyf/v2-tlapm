(**
   This file contains helper function to resolve references to expressions.
*)
open Expr_ds


(** retrieves the entry with the given id from the term_db and transforms it.
    the dereference function use Expr_utils.unpack_* as transformer.
*)
val find_entry : (entry -> 'a) -> term_db -> int -> 'a

(** If the passed operator is a reference, it will be retrieved from the
    term_db. Otherwise, its content is directly extracted. *)
val dereference_user_defined_op : term_db -> user_defined_op -> user_defined_op_

(** If the passed formal parameter is a reference, it will be retrieved from the
    term_db. Otherwise, its content is directly extracted. *)
val dereference_formal_param : term_db -> formal_param -> formal_param_

(** If the passed operator declaration is a reference, it will be retrieved from
    the term_db. Otherwise, it is directly extracted. *)
val dereference_op_decl : term_db -> op_decl -> op_decl_

(** If the passed operator definition is a reference, it will be retrieved from
    the term_db. Otherwise, it is directly extracted. *)
val dereference_module_instance : term_db -> module_instance -> module_instance_

(** If the passed operator definition is a reference, it will be retrieved from
    the term_db. Otherwise, it is directly extracted. *)
val dereference_user_defined_op : term_db -> user_defined_op -> user_defined_op_

(** If the passed theorem is a reference, it will be retrieved from the
    term_db. Otherwise, it is directly extracted. *)
val dereference_theorem : term_db -> theorem -> theorem_

(** If the passed theorem is a reference, it will be retrieved from the
    term_db. Otherwise, it is directly extracted. *)
val dereference_assume : term_db -> assume -> assume_

(** If the passed theorem is a reference, it will be retrieved from the
    term_db. Otherwise, it is directly extracted. *)
val dereference_module : term_db -> mule -> mule_

(** compare two formal params after dereferencing *)
val compare_modulo_deref_formal_param :
  term_db -> ?cmp:(formal_param_ -> formal_param_ -> bool)
  -> formal_param -> formal_param -> bool
