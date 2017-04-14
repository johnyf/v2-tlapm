open Commons
open Expr_ds
open Expr_map

(** removes all symbolic representations of proof steps
    (TAKE, PICK, SUFFICES, etc.) and introduces the explicit steps
    instead
*)
val expr_parse_theorems_context : context -> context
