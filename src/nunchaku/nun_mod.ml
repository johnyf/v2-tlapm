open Sexplib.Type
open Format
open CCFormat



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

let rec sexp_to_term t = match t with
  | Atom s -> Var s 	
  | List ((Atom "?__")::l) -> App ("UNDEFINED",(List.map sexp_to_term l))
  | List ((Atom s)::l) -> App (s,(List.map sexp_to_term l))
  | List [List [Atom v; Atom t]] -> Var ("("^v^" : "^t^")")
  | _ -> failwith "unparsed term"

let rec sexp_to_type t = match t with
  | Atom s -> s 	
  | List [(Atom "->");a;b] -> (sexp_to_type a)^" -> "^(sexp_to_type b)
  | _ -> failwith "unparsed type"

let rec sexp_to_string t =
  let rec list_to_string list =
    match list with
    | [] -> ""
    | [x] -> sexp_to_string x
    | t::q -> (sexp_to_string t)^" "^(list_to_string q)
  in 
  match t with
  | Atom s -> s 	
  | List l -> "("^(list_to_string l)^")"

let sexp_to_type_model_entry t = match t with
  | ((Atom name)::[List l]) ->
    let rec unroll m = match m with
      | [] -> []
      | (Atom s)::m2 -> s::(unroll m2)
      | _ -> failwith "unroll type var failed"
    in
    Type (name,(unroll l))
  | _ -> failwith "type_to_model failed"

let sexp_to_name t = match t with
  | Atom name -> name
  | List [Atom n1;Atom n2] -> n1^"__"^n2 (* polymorphic case *)
  | List [(Atom "_witness_of");List [_;List [List [name;_]];_]] -> "witness_of "^(sexp_to_string name) (* skolems *)
  | _ -> failwith "sexp_to_name failed"

let sexp_to_val_model_entry t = match t with
  | [name;value] -> Const (sexp_to_name name, sexp_to_term value)
  | _ -> failwith "val_to_model failed"

let rec unroll_vars v = match v with
  | [] -> []
  | (List [Atom var_name; var_type])::tl -> (var_name, sexp_to_type var_type)::(unroll_vars tl)
  | _ -> failwith "unroll fun var failed"

let rec unroll_conditions c = match c with
  | [] -> []
  | (List [(Atom "=");(Atom v);(List ((Atom "fun")::tl_fun))])::tl -> (v,unroll_fun_term tl_fun)::(unroll_conditions tl)
  | (List [(Atom "=");(Atom v);t])::tl -> (v,sexp_to_term t)::(unroll_conditions tl)
  | _ -> failwith "unroll fun conditions failed"

and match_conditions c = match c with
  | (Atom "and")::tl -> unroll_conditions tl
  | _ -> unroll_conditions [Sexplib.Type.List c]

and unroll_values v = match v with
  | List ((Atom "if")::(List conditions)::(then_)::(tl)::[]) ->
    let dt = unroll_values tl in
    {
      cases = (match_conditions conditions,sexp_to_term then_)::dt.cases ;
      else_ = dt.else_
    }
  | Atom _ -> {cases = []; else_= sexp_to_term v}
  | List _ -> {cases = []; else_= sexp_to_term v}

and unroll_fun_term l = match l with
  | [(List vars);values] -> Fun ((unroll_vars vars),(unroll_values values))
  | _ -> failwith "ERROR unroll_fun_term failed"   

let unroll_var v = match v with
  | [Atom var_name; var_type] -> (var_name, sexp_to_type var_type)
  | _ -> failwith "ERROR unroll var failed"

let rec sexp_to_fun_model_entry name var_acc t = match t with
  | (List [(Atom "fun"); List var;tl]) -> sexp_to_fun_model_entry name ((unroll_var var)::var_acc) tl
  | _ -> Const (name, (Fun (var_acc,(unroll_values t))))

let sexp_to_model_entry t = match t with
  | List ((Atom "type")::tl) -> sexp_to_type_model_entry tl 
  | List [(Atom "val");name;(List ((Atom "fun")::tl))]  -> sexp_to_fun_model_entry (sexp_to_name name) [] (List ((Atom "fun")::tl)) 
  | List ((Atom "val")::tl)  -> sexp_to_val_model_entry tl
  | _ -> failwith "sexp_to_model_entry failed"

let sexp_to_model l = List.map sexp_to_model_entry l

let nun_sexp_to_nun_mod t =
  match t with
  | Atom "UNSAT" -> UNSAT
  | Atom "UNKNOWN" -> UNKNOWN
  | Atom "TIMEOUT" -> TIMEOUT
  | List (Atom "SAT"::[List l]) -> SAT (sexp_to_model l)
  | _ -> failwith "Unknown structure"





(** Printer **)

let rec list_to_string print_one separator list =
  match list with
  | [] -> ""
  | [x] -> print_one x
  | t::q -> (print_one t)^separator^(list_to_string print_one separator q)

let comma = ", "
let andand = " && " 
let newline = " \n"
let space = " "

let type_vars_to_string = list_to_string (fun x -> x) comma
let fun_vars_to_string = list_to_string (fun (s1,s2) -> "("^s1^" : "^s2^")") comma

let rec fun_conditions_to_string fc =
  let f (s,t) = match t with
    | Fun _ -> "("^s^" = (fun "^(term_to_string t)^"))"
    | _ -> "("^s^" = "^(term_to_string t)^")"
  in
  list_to_string f andand fc

and term_to_string t = match t with
  | Var s -> s
  | App (s,l) -> s^" ("^(list_to_string term_to_string comma l)^")"
  | Fun (vars,dt) -> (fun_vars_to_string vars)^"\n "^(dt_to_string dt)

and dt_to_string dt = match dt.cases with
  | []                ->
    "\t else : "^(term_to_string dt.else_)
  | (cond_l,then_)::q ->
    "\t if "^(fun_conditions_to_string cond_l)^" then : "^(term_to_string then_)^"\n"^(dt_to_string {cases = q; else_=dt.else_})

let model_entry_to_string m_e =
  match m_e with
  | Type (name,vars) -> "type "^name^" : "^(type_vars_to_string vars)
  | Const (name,Fun (f,g)) -> "fun "^name^" : "^(term_to_string (Fun (f,g)))
  | Const (name,value) -> "val "^name^" : "^(term_to_string value)

let model_to_string model =
  list_to_string model_entry_to_string newline model

let nun_mod_to_string t =
  match t with
  | UNSAT -> "UNSAT"
  | UNKNOWN -> "UNKNOWN"
  | TIMEOUT -> "TIMEOUT"
  | SAT model -> ("SAT (\n"^(model_to_string model)^")")

let print_nun_mod output_file mod_tree =
  (*val: string -> mod_tree -> unit*)
  let oc = open_out output_file in
  let fft = formatter_of_out_channel oc in
  fprintf fft "%s" (nun_mod_to_string mod_tree);
  fprintf fft "@.%!";
  print_flush ();
  close_out oc




