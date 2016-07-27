open Nun_mod
       
type term =
  | Var of string
  | App of string * term list
  | Fun of (string*string) list * decision_tree			 

and decision_tree =
  {
    cases: ((string * term) list * term) list;
    else_ : term;
  }

type model = 
  {
    u    : string list ;
    var  : (string * string) list ;
    mem  : (string * (string list)) list ;
    funs : (string * term) list
  }
    
type tla_mod = UNSAT | UNKNOWN | TIMEOUT | SAT of model

                                          
(** Translation **)

val nun_mod_to_tla_mod : nun_mod -> tla_mod

                                                 
(** Printer **)
                                                 
val fmt_tla_mod : Format.formatter -> tla_mod -> unit
                                     
val print_tla_mod : string -> tla_mod -> unit
