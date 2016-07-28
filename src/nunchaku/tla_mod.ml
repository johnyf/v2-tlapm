open Nun_mod
open Format
       
(** Definition **)
       
type model = 
  {
    u    : string list ;
    var  : (string * string) list ;
    mem  : (string * (string list)) list ;
    funs : (string * string list * decision_tree) list
  }

 and decision_tree =
  {
    cases: ((string * string) list * string) list;
    else_ : string;
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

let nun_to_tla_dt ({cases; else_}:Nun_mod.decision_tree) =
  let one_case case =
    let (cond_l, then_) = case
    in
    let f a = let (s,t) = a in (s,Nun_mod.term_to_string t) in
    (List.map f cond_l, Nun_mod.term_to_string then_)
  in
  {cases = List.map one_case cases; else_ = Nun_mod.term_to_string else_}
                                    
let add_var name (value:Nun_mod.term) {u; var; mem; funs} =
  let new_var = match value with
  | Var v -> (name, v)
  |   _   -> (name, "ERROR add_var failed")  
  in
  {u = u; var = new_var::var; mem = mem; funs = funs}

let rec add_mem v0 v1 acc = match acc with
  | [] -> [(v0,[v1])]
  | (v,l)::q when v=v0 -> (v0,v1::l)::q
  | t::q -> t::(add_mem v0 v1 q)
    
let rec set_mem_ cases acc = match cases with
  | [] -> acc
  | (_, "false")::q -> set_mem_ q acc
  | ([("v_0",v0);("v_1",v1)], "true")::q -> set_mem_ q (add_mem v0 v1 acc)
  | ([("v_1",v1);("v_0",v0)], "true")::q -> set_mem_ q (add_mem v0 v1 acc)
  | _ -> failwith "mem_raw parsing failed"                         

let set_mem f = let (_,_,{cases;_}) = f in set_mem_ cases []
                  
let nun_to_tla_fun name fvar fdt = (name, List.map fst fvar, nun_to_tla_dt fdt)

let add_fun name fvar fdt {u; var; mem; funs} = match name with
  (* | "mem" -> {u; var; mem; funs} *)
  (* | "trans_mem" -> {u; var; mem; funs} *)
  | "mem_raw" -> let mem' = nun_to_tla_fun name fvar fdt in
                 {u=u; var=var; mem=set_mem mem'; funs=(mem')::funs}
  (* TODO : unique_unsafe__??? *)
  | _ -> {u = u; var = var; mem = mem; funs = (nun_to_tla_fun name fvar fdt)::funs}    
  
let add_to_mod nun_model_entry tla_model = match nun_model_entry with
  | Type ("alpha_u",l) -> set_u l tla_model
  | Const (name,Fun (var,dt)) -> add_fun name var dt tla_model
  | Const (n,v) -> add_var n v tla_model
  | _ -> failwith "ERROR add_to_mod"
             
let nun_model_to_tla_model nun_model =
  let model = empty_model in
  List.fold_right add_to_mod nun_model model
    
let nun_mod_to_tla_mod (nun_mod:nun_mod) = match nun_mod with
  | UNSAT     -> UNSAT
  | UNKNOWN   -> UNKNOWN
  | TIMEOUT   -> TIMEOUT
  | SAT model -> SAT (nun_model_to_tla_model model)

                     


                     
(** Printer **)

let rec fmt_list fmt_one separator pp list = match list with
  |  []  -> ();
  |  [x] -> fprintf pp "%a" fmt_one x
  | t::q -> fprintf pp "%a%s%a" fmt_one t separator (fmt_list fmt_one separator) q
                     
let rec fmt_var pp var =
  let fmt_one pp v = let (name,value) = v in fprintf pp "@.%s : %s" name value in
  fmt_list fmt_one ", " pp var
           
let rec fmt_u pp u =
  let fmt_one pp v = fprintf pp "@.%s" v in
  fmt_list fmt_one ", " pp u
           
let rec fmt_vars pp vars =
  let fmt_one pp v = fprintf pp "%s" v in
  fmt_list fmt_one ", " pp vars

let rec fmt_conds pp var =
  let fmt_one pp v = let (name,value) = v in fprintf pp "%s = %s" name value in
  fmt_list fmt_one " && " pp var

let fmt_case pp case =
  let (conds,then_) = case
  in fprintf pp "if (%a): %s" fmt_conds conds then_
                    
let rec fmt_cases pp cases =
  let fmt_one pp v = fprintf pp "@.%a" fmt_case v in
  fmt_list fmt_one "; " pp cases

let fmt_dt pp {cases; else_} =
  fprintf pp "%a@.else:%s" fmt_cases cases else_
  
let fmt_fun pp f =
  let (name, vars, dt) = f in
  fprintf pp "@.fun %s (%a) : %a@." name fmt_vars vars fmt_dt dt

let fmt_funs pp fs = ignore(List.map (fmt_fun pp) fs)
          
let fmt_set pp set = 
  let fmt_one pp v = fprintf pp "%s" v in
  fmt_list fmt_one "; " pp set

let fmt_mem pp mem = 
  let fmt_one pp v = let (set,elements) = v in fprintf pp "@.%s = {%a}" set fmt_set elements in
  fmt_list fmt_one "; " pp mem

let tla_model_to_string pp {u; var; mem; funs} =
  fprintf pp "@.";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "U = {" fmt_u u "}";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "VARS = {" fmt_var var "}";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "FUNCTIONS = {" fmt_funs funs "}";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "MEM = {" fmt_mem mem "}"

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

let tla_mod_to_string tla_mod =  
  let fft = str_formatter in
  Format.fprintf fft "%a" fmt_tla_mod tla_mod;
  flush_str_formatter ()
                      
