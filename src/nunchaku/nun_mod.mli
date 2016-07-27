open Nun_sexp
       
(** Definition **)
       
type term =
  | Var of string
  | App of string * term list
  | Fun of (string*string) list * decision_tree			 

and decision_tree =
  {
    cases: ((string * term) list * term) list;
    else_ : term;
  }

type model_entry =
  | Type of string * string list
  | Const of string * term

type model = model_entry list

type nun_mod = UNSAT | UNKNOWN | TIMEOUT | SAT of model

                                          
(** Translation **)

val nun_sexp_to_nun_mod : nun_sexp -> nun_mod

                                                 
(** Printer **)
                                                 
val nun_mod_to_string : nun_mod -> string
                                     
val print_nun_mod : string -> nun_mod -> unit
