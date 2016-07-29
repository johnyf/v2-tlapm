open Nun_mod
open Format
       
(** Definition **)
       
type model = 
  {
    var : (string * string) list ;
    mem : (string * (string list)) list ;
    app : (string * string list * decision_tree) option;
    dom : (string * string list * decision_tree) option
  }

 and decision_tree =
  {
    cases: ((string * string) list * string) list;
    else_ : string;
  }
    
type tla_mod = VALID | UNKNOWN | TIMEOUT | REFUTED of model




                                                    
(** Translation **)

let empty_decision_tree = {cases = []; else_ = ""}
                            
let empty_model =
  {
    var = [] ;
    mem = [] ;
    app = None ;
    dom = None
  }

let set_var {var; mem; app; dom} var' = {var=var'; mem; app; dom}

let set_mem {var; mem; app; dom} mem' = {var; mem=mem'; app; dom}
    
let set_app {var; mem; app; dom} app' = {var; mem; app=app'; dom}
    
let set_dom {var; mem; app; dom} dom' = {var; mem; app; dom=dom'}
                                          
let nun_to_tla_dt ({cases; else_}:Nun_mod.decision_tree) =
  let one_case case =
    let (cond_l, then_) = case
    in
    let f a = let (s,t) = a in (s,Nun_mod.term_to_string t) in
    (List.map f cond_l, Nun_mod.term_to_string then_)
  in
  {cases = List.map one_case cases; else_ = Nun_mod.term_to_string else_}
                                    
let add_var name (value:Nun_mod.term) model =
  let new_name =
    if Str.string_match (Str.regexp "witness_of[.]*") name 0
    then "witness"
    else name
  in
  let new_var = match value with
  | Var v -> (new_name, v)
  |   _   -> (name, "ERROR add_var failed")  
  in
  set_var model (new_var::model.var)
          
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

let set_mem_ f = let (_,_,{cases;_}) = f in set_mem_ cases []
                  
let nun_to_tla_fun name fvar fdt = (name, List.map fst fvar, nun_to_tla_dt fdt)

let add_fun name fvar fdt model = match name with
  | s when s="mem" || s = "trans_mem" || s = "unique_unsafe__u" -> model
  | "mem_raw" -> let mem' = nun_to_tla_fun name fvar fdt in
                 set_mem model (set_mem_ mem')
  | "app" -> model                    (* TODO *)
  | "dom" -> model                   (* TODO *)
  | _ -> failwith "Add_fun failed due to unmatched function"
     (* | _ -> {var = var; mem = mem; funs = (nun_to_tla_fun name fvar fdt)::funs} (\* FAIL if not matched *\) *)
  
let add_to_mod nun_model_entry tla_model = match nun_model_entry with
  | Type ("alpha_u",l) -> tla_model
  | Const (name,Fun (var,dt)) -> add_fun name var dt tla_model
  | Const (n,v) -> add_var n v tla_model
  | _ -> failwith "ERROR add_to_mod"
             
let nun_model_to_tla_model nun_model =
  let model = empty_model in
  List.fold_right add_to_mod nun_model model
    
let nun_mod_to_tla_mod (nun_mod:nun_mod) = match nun_mod with
  | UNSAT     -> VALID
  | UNKNOWN   -> UNKNOWN
  | TIMEOUT   -> TIMEOUT
  | SAT model -> REFUTED (nun_model_to_tla_model model)

                     


                     
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

(* let fmt_funs pp fs = ignore(List.map (fmt_fun pp) fs) *)
          
let fmt_app pp app = match app with
  | Some f -> fmt_fun pp f
  | None   -> ()

let fmt_dom pp dom = match dom with
  | Some f -> fmt_fun pp f
  | None   -> ()
                
let fmt_set pp set = 
  let fmt_one pp v = fprintf pp "%s" v in
  fmt_list fmt_one "; " pp set

let fmt_mem pp mem = 
  let fmt_one pp v = let (set,elements) = v in fprintf pp "@.%s = {%a}" set fmt_set elements in
  fmt_list fmt_one "; " pp mem

let tla_model_to_string_verbose pp model =
  fprintf pp "@.";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "VARS = {" fmt_var model.var "}";
  fprintf pp "%s@[<2>%a@.@.%a@]@.%s@.@." "FUNCTIONS = {" fmt_app model.app fmt_dom model.dom "}";
  fprintf pp "%s@[<2>%a@]@.%s@.@." "MEM = {" fmt_mem model.mem "}"

let rec get_set x mem = match mem with
  | [] -> []
  | (v,l)::q when v=x -> l
  | t::q -> (get_set x q)

let rec fmt_tla_set pp set_mem =
  let (set,mem) = set_mem in
  match set with
  | [] -> ()
  | [x] -> fprintf pp "{%a}" fmt_tla_set (get_set x mem, mem)
  | t::q -> fprintf pp "{%a}, " fmt_tla_set (get_set t mem, mem);
            fmt_tla_set pp (q,mem)
              
let rec fmt_var_with_mem pp var_mem =
  let (var,mem) = var_mem in
  match var with
  | [] -> ()
  | (n,v)::q -> fprintf pp "@.%s = {%a}" n fmt_tla_set ((get_set v mem), mem);
                fmt_var_with_mem pp (q, mem)
          
let tla_model_to_string pp model =
  fprintf pp "@[%a@]@." fmt_var_with_mem (model.var, model.mem);
  fprintf pp "@[%a@]" fmt_app model.app;
  fprintf pp "@[%a@]" fmt_dom model.dom
          
let fmt_tla_mod pp tla_mod =
  match tla_mod with
  | VALID -> fprintf pp "%s" "VALID"
  | UNKNOWN -> fprintf pp "%s" "UNKNOWN"
  | TIMEOUT -> fprintf pp "%s" "TIMEOUT"
  | REFUTED model -> fprintf pp "%s@.@[<2>%a@]" "Countermodel:" tla_model_to_string model
                                  
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
