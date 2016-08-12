open Commons
open Expr_ds
open Expr_map

type 'a ptacc = term_db option * 'a

(** The map removes all symbolic representations of proof steps
    (TAKE, PICK, SUFFICES, etc.) and introduces the explicit steps
    instead *)
class ['a] expr_parse_theorems : object
  inherit ['a ptacc] expr_map
end


(** applies expr_parse_theorems#context to a context and unfolds the result
*)
val expr_parse_theorems_context : context -> context
