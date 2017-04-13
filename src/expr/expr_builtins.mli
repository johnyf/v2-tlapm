open Expr_ds

module Builtin : sig
  exception BuiltinNotFound of string * string

  type builtin_symbol =
    | TRUE
    | FALSE
    | NOT
    | AND
    | OR
    | IMPLIES
    | FORALL
    | EXISTS
    | BFORALL
    | BEXISTS
    | EQ
    | NEQ
    | PRIME
    | TFORALL
    | TEXISTS
    | BOX
    | DIAMOND
    | WF
    | SF
    | FUNAPP
    | SET_ENUM
    | SQ_BRACK
    | ANG_BRACK

  val get : term_db -> builtin_symbol -> builtin_op
  (** Fetches a builtin symbol from the term db. Raises BuiltinNotFound if it is
      not in the db. *)
  val complete_builtins : term_db -> term_db
  (** Extends the given term db by all missing builtins. *)
end
