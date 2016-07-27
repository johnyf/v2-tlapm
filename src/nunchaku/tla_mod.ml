open Nun_mod
open Format
       
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

type model = 
  {
    u    : string list ;
    var  : (string * string) list ;
    mem  : (string * (string list)) list ;
    funs : (string * term) list
  }
    
type tla_mod = UNSAT | UNKNOWN | TIMEOUT | SAT of model




                                                    
(** Translation **)
                                                    
let empty_model =
  {
    u    = [] ;
    var  = [] ;
    mem  = [] ;
    funs = []
  }

let set_u l {u; var; mem; funs} = {u = l; var = var; mem = mem; funs = funs}
    
let add_to_mod nun_model_entry tla_model = match nun_model_entry with
  | Type ("alpha_u",l) -> set_u l tla_model
  | _ -> tla_model
             
let nun_model_to_tla_model nun_model =
  let model = empty_model in
  List.fold_right add_to_mod nun_model model
    
let nun_mod_to_tla_mod (nun_mod:nun_mod) = match nun_mod with
  | UNSAT     -> UNSAT
  | UNKNOWN   -> UNKNOWN
  | TIMEOUT   -> TIMEOUT
  | SAT model -> SAT (nun_model_to_tla_model model)

                     


                     
(** Printer **)

let rec fmt_u pp u = match u with
  |  []  -> ();
  |  [x] -> fprintf pp "@.%s" x
  | t::q -> fprintf pp "@.%s%s%a" t ", " fmt_u q
                     
let tla_model_to_string pp {u; var; mem; funs} =
  fprintf pp "%s@[<2>%a@]@.%s" "U = {" fmt_u u "}"
  (* fprintf pp "%s@[<2>%a@]@.%s" "VARS = {" fmt_var var "}" *)

let fmt_tla_mod pp tla_mod =
  match tla_mod with
  | UNSAT -> fprintf pp "%s" "UNSAT"
  | UNKNOWN -> fprintf pp "%s" "UNKNOWN"
  | TIMEOUT -> fprintf pp "%s" "TIMEOUT"
  | SAT model -> fprintf pp "%s(@.@[<2>%a@]@.)" "SAT" tla_model_to_string model
                                  
let print_tla_mod output_file tla_mod =
  let oc = open_out output_file in
  let fft = Format.formatter_of_out_channel oc in
  Format.fprintf fft "%a" fmt_tla_mod tla_mod;
  Format.fprintf fft "@.%!";
  print_flush ();
  close_out oc
