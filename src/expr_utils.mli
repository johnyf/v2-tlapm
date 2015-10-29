(** Some basic operations on the expression datastructures. *)
open Commons
open Expr_ds

(** Extracts the formal parameter from an entry *)
val unpack_fp_entry : entry -> formal_param_

(** Extracts the module from an entry *)
val unpack_mod_entry : entry -> mule_

(** Extracts the operator declaration from an entry *)
val unpack_opdecl_entry : entry -> op_decl_

(** Extracts the operator definition from an entry *)
val unpack_opdef_entry : entry -> op_def

(** Extracts the theorem from an entry *)
val unpack_thm_entry : entry -> theorem_

(** Extracts the assume from an entry *)
val unpack_assume_entry : entry -> assume_

(** Extracts the ap subst from an entry *)
val unpack_apsubst_entry : entry -> ap_subst_in

(** Extracts a location from an expression *)
val extract_location : expr -> location

(** Extracts a location from an expression *)
val extract_level : expr -> level option

(** Wraps an expression into an assume-prove with empty assumptions *)
val assume_prove_from_expr : expr -> bool -> assume_prove
