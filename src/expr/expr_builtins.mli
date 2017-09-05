open Expr_ds
open CCFormat

module Builtin : sig
  exception BuiltinNotFound of string * string

  type builtin_symbol =
    (* logical operators *)
    | TRUE
    | FALSE
    | EQ
    | NEQ
    | NOT
    | AND
    | OR
    (* quantifiers *)
    | IMPLIES
    | FORALL
    | EXISTS
    | BFORALL
    | BEXISTS
    (* temporal operators *)
    | PRIME
    | TFORALL
    | TEXISTS
    | BOX
    | DIAMOND
    | SQ_BRACK
    | ANG_BRACK
    | WF
    | SF
    (* tuples, functions, records *)
    | TUPLE
    | FUN_APP
    | FUN_CONSTR
    | RCD_CONSTR
    | SET_ENUM
    | SET_MEMBER
    | IF_THEN_ELSE

  val get : term_db -> builtin_symbol -> builtin_op
  (** Fetches a builtin symbol from the term db. Raises BuiltinNotFound if it is
      not in the db. *)

  val string_of_builtin : builtin_symbol -> string
  (** obtain the sany string representation of builtin *)
  val builtin_of_string : string -> builtin_symbol
  (** parse a string to a builtin symbol *)
  val pp : builtin_symbol printer
  (** printer for builtin *)
  val complete_builtins : term_db -> term_db
  (** Extends the given term db by all missing builtins. *)
end
