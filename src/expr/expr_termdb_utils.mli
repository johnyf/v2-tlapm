open List
open Util
open Expr_ds
open Expr_visitor

(** The module Expr_termdb_utils allows maintenance of the term_db:
     It provides consistency and redundancy checks and allows to
     extend the term_db by new entries. Newly created terms will have
     a reference id >= 10000.
*)

module Tdb = IntMap
type tdb = entry Tdb.t

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

val tdb_of_termdb : term_db -> tdb
val termdb_of_tdb : tdb -> term_db

module DeepTraversal : sig
  (** This module provides traversal classes for expressions which follow
      references.
  *)

  (** Finds all the free variables in an expression *)
  val free_variables : term_db -> expr -> op_decl list

  (** Finds all the bound variables in an expression *)
  val bound_variables : term_db -> expr -> formal_param list

  (** Finds all formal parameters in an expression *)
  val formal_params : term_db -> expr -> formal_param list

  (** Extracts the formal parameter of the bound variable from a binder *)
  val formal_params_from_binder : term_db -> binder -> formal_param list

  (** DTacc provides the global term database, the set of visited ids and an
      inner accumulator for tdb_visitor *)
  type 'a dtacc = DTAcc of term_db * IntSet.t * 'a

  (* visitor with added term db and dereferencing *)
  class ['a] tdb_visitor : object
    inherit ['a dtacc] visitor

    method dtacc_add_visited   : 'a dtacc -> IntSet.elt -> 'a dtacc
    method dtacc_inner_acc     : 'a dtacc -> 'a
    method dtacc_set_inner_acc : 'a dtacc -> 'a -> 'a dtacc
    method dtacc_set_term_db   : 'a dtacc -> term_db -> 'a dtacc
    method dtacc_term_db       : 'a dtacc -> term_db
    method dtacc_visited       : 'a dtacc -> IntSet.t
  end

  module FP_comparable : sig
    type t = formal_param
    val compare : 'a -> 'a -> int
  end

  module FP_Set : Set.S with type elt = formal_param

  class formal_param_visitor : object
    inherit [FP_Set.t] tdb_visitor
  end

  val formal_param_visitor_obj : formal_param_visitor
end
