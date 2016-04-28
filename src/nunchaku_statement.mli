open Format

type nun_declaration = Declaration of string * string
type nun_axiom = Axiom of string
type nun_goal = Goal of string
type nun_comment = Comment of string
type nun_include = Include of string

type nun_statement = nun_goal * nun_declaration list * nun_axiom list * nun_comment list * nun_include list

val print_statement : formatter -> nun_statement -> unit
