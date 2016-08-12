open Commons
(** This module contains exceptions which are thrown by backends. **)

exception UnhandledLanguageElement of prover * string
(** A backend usually supports only a subset of TLA. This exception is used to
    signal such an unhandled structure. Since enumerating all unhandled types
    is  unfeasible, a string description of the element is passed. *)

exception ExternalToolFailed of prover * string * string
(** A backend might fail for some reason - the first argument - and pass
    additional debug output - the second argument.
*)
