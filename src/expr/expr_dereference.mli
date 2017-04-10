(**
   This file contains helper function to resolve references to expressions.
*)
open Expr_ds

module Deref : sig
  (** retrieves the entry with the given id from the term_db and transforms it.
      the dereference function use Expr_utils.unpack_* as transformer.
  *)
  val find_entry : (entry -> 'a) -> term_db -> int -> 'a

  (** If the passed operator is a reference, it will be retrieved from the
      term_db. Otherwise, its content is directly extracted. *)
  val user_defined_op : term_db -> user_defined_op -> user_defined_op_

  (** If the passed operator is a reference, it will be retrieved from the
      term_db. Otherwise, its content is directly extracted. *)
  val builtin_op : term_db -> builtin_op -> builtin_op_

  (** If the passed formal parameter is a reference, it will be retrieved from the
      term_db. Otherwise, its content is directly extracted. *)
  val formal_param : term_db -> formal_param -> formal_param_

  (** If the passed operator declaration is a reference, it will be retrieved from
      the term_db. Otherwise, it is directly extracted. *)
  val op_decl : term_db -> op_decl -> op_decl_

  (** If the passed operator definition is a reference, it will be retrieved from
      the term_db. Otherwise, it is directly extracted. *)
  val module_instance : term_db -> module_instance -> module_instance_

  (** If the passed operator definition is a reference, it will be retrieved from
      the term_db. Otherwise, it is directly extracted. *)
  val user_defined_op : term_db -> user_defined_op -> user_defined_op_

  (** If the passed theorem is a reference, it will be retrieved from the
      term_db. Otherwise, it is directly extracted. *)
  val theorem : term_db -> theorem -> theorem_

  (** If the passed theorem is a reference, it will be retrieved from the
      term_db. Otherwise, it is directly extracted. *)
  val assume : term_db -> assume -> assume_


  (** If the passed theorem is a reference, it will be retrieved from the
      term_db. Otherwise, it is directly extracted. *)
  val theorem_def : term_db -> theorem_def -> theorem_def_

  (** If the passed theorem is a reference, it will be retrieved from the
      term_db. Otherwise, it is directly extracted. *)
  val assume_def : term_db -> assume_def -> assume_def_

  (** If the passed theorem is a reference, it will be retrieved from the
      term_db. Otherwise, it is directly extracted. *)
  val mule : term_db -> mule -> mule_

  (** compare two formal params after dereferencing *)
  val compare_modulo_deref_formal_param :
    term_db -> ?cmp:(formal_param_ -> formal_param_ -> bool)
    -> formal_param -> formal_param -> bool
end
