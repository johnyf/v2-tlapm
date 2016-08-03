open Format

type nun_var = string * string
type nun_operator = Operator of string
type nun_declaration = Declaration of nun_var
type nun_comment = Comment of string
type nun_include = Include of string
type nun_binder = Forall | Exists
type nun_operands = nun_expr
and nun_expr =
  | Op of nun_operator * nun_operands list
  | Binder of nun_binder * nun_var * nun_expr
  | Atom of string
type nun_axiom = Axiom of nun_expr
type nun_goal = Goal of nun_expr

type nun_statement = nun_goal * nun_declaration list * nun_axiom list * nun_comment list * nun_include list

val print_statement : formatter -> nun_statement -> unit
