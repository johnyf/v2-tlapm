type statement =
  | Declaration of string * string
  | Axiom of string
  | Goal of string
  | Comm of string
  | Include of string
  | Else of string

val print_statement : Format.formatter -> statement -> unit
