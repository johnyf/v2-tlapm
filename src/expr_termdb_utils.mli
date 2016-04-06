open List
open Expr_ds
open Expr_visitor

(** The module Expr_termdb_utils allows maintenance of the term_db:
     It provides consistency and redundancy checks and allows to
     extend the term_db by new entries. Newly created terms will have
     a reference id >= 10000.
*)

(** Extracts a list of all reference ids in the term db. *)
val get_ids : term_db -> int list

(** Returns the non-existing entries of the term_db. *)
val inconsistent_entries : term_db -> int list

(** Checks if any entries in the term_db refer to non-existing entries. *)
val is_consistent : term_db -> bool

(** Returns those term_db entries whose id is not mentioned
    anywhere else in the db.
*)
val unmentioned_entries : term_db -> int list

(** Checks if the term_db contains entries whose id are not mentioned
    anywhere else in the db.
*)
val has_unmentioned_entries : term_db -> bool

(** Removes unmentioned entries from the term_db, until all entries are
    mentioned. *)
val prune_term_db : term_db -> term_db

(** creates a reference to a formal_param and enters into the term_db,
     if necessary *)
val mkref_formal_param : ?compare:(entry -> entry -> bool) -> term_db
                         -> formal_param_ -> (term_db * formal_param)
(** creates a reference to a module and enters into the term_db, if necessary *)
val mkref_mule : ?compare:(entry -> entry -> bool) -> term_db
                 -> mule_ -> (term_db * mule)

(** creates a reference to an operator declaration and enters into the term_db,
    if necessary *)
val mkref_opdec : ?compare:(entry -> entry -> bool) -> term_db
                  -> op_decl_ -> (term_db * op_decl)

(** creates a reference to a user defined operator and enters into the term_db,
    if necessary *)
val mkref_user_defined_op : ?compare:(entry -> entry -> bool) -> term_db
                            -> user_defined_op_ -> (term_db * user_defined_op)

(** creates a reference to a module instance and enters into the term_db,
    if necessary *)
val mkref_module_instance : ?compare:(entry -> entry -> bool) -> term_db
                            -> module_instance_ -> (term_db * module_instance)
