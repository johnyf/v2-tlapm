(** Some basic operations on the expression datastructures. *)
open Commons
open Simple_expr_ds

(** Extracts the formal parameter from an entry *)
val unpack_fp_entry : simple_entry -> simple_formal_param_

(** Extracts the operator declaration from an entry *)
val unpack_opdecl_entry : simple_entry -> simple_op_decl_

(** Extracts the operator definition from an entry *)
val unpack_opdef_entry : simple_entry -> simple_op_def

(** Extracts a location from an expression *)
val extract_location : simple_expr -> location

(** Extracts a location from an expression *)
val extract_level : simple_expr -> level option

(** Wraps an expression into an assume-prove with empty assumptions *)
val assume_prove_from_expr : bool -> simple_expr -> simple_assume_prove

(** Finds all the free variables in an expression *)
val free_variables : simple_expr -> simple_op_decl list
