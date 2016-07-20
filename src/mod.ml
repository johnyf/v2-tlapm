open Sexplib.Type
open Format
open CCFormat


       
(* AST DEFINITION *)
       
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

type mod_tree = UNSAT | UNKNOWN | SAT of model

	      
(* SEXP TO MOD_TREE *)
		
let rec sexp_to_term t = match t with
  | Atom s -> Var s 	
  | List ((List [Atom "?__";Atom "_"])::l) -> Var "UNKNOWNED"
  | List ((Atom s)::l) -> App (s,(List.map sexp_to_term l))
  | _ -> Var "ERROR unparsed term"
  (* | _ -> failwith "unparsed term" *)

let rec sexp_to_type t = match t with
    | Atom s -> s 	
    | List [(Atom "->");a;b] -> (sexp_to_type a)^" -> "^(sexp_to_type b)
    | _ -> "ERROR unparsed type"
    (* | _ -> failwith "unparsed type" *)
 		    
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
       | _ -> ["ERROR unparsed term"]
       (* | _ -> failwith "unroll type var failed" *)
     in
     Type (name,(unroll l))
  | _ -> Type ("ERROR type_to_model failed", [])
  (* | _ -> failwith "type_to_model failed" *)

let sexp_to_name t = match t with
  | Atom name -> name
  | List [Atom n1;Atom n2] -> n1^"__"^n2 (* polymorphic case *)
  | List [(Atom "_witness_of");(prop)] -> "witness_of "^(sexp_to_string prop)
  | _ -> "ERROR sexp_to_name failed"
	      
let sexp_to_val_model_entry t = match t with
  | [name;value] -> Const (sexp_to_name name, sexp_to_term value)
  | _ -> Const ("ERROR val_to_model failed",Var "")
  (* | _ -> failwith "val_to_model failed" *)

let rec unroll_vars v = match v with
  | [] -> []
  | (List ([(Atom n);t]))::m2 -> (n, sexp_to_type t)::(unroll_vars m2)
  | _ -> ["ERROR unroll fun var failed",""]
(* | _ -> failwith "unroll fun var failed" *)
	   
let rec unroll_conditions c = match c with
  | [] -> []
  | (List [(Atom "=");(Atom v);(List [(Atom "fun");List vars;values]) ])::tl ->
     let vars_term = unroll_vars vars in
     let val_term = unroll_values values in
     (v,Fun (vars_term,val_term))::(unroll_conditions tl)
  | (List [(Atom "=");(Atom v);t])::tl -> (v,sexp_to_term t)::(unroll_conditions tl)
  | _ -> ["ERROR unroll fun conditions failed",Var ""]
(* | _ -> failwith "unroll fun conditions failed" *)

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
(* | _ -> failwith "unroll values fun_to_model failed" *)

let sexp_to_fun_model_entry name t = match t with 
  | [(List vars);values] -> Const (name,Fun ((unroll_vars vars),(unroll_values values)))
  | _ -> failwith "type_to_model failed"
  
let sexp_to_model_entry t = match t with
  | List ((Atom "type")::tl) -> sexp_to_type_model_entry tl 
  | List [(Atom "val");name;(List ((Atom "fun")::tl))]  -> sexp_to_fun_model_entry (sexp_to_name name) tl 
  | List ((Atom "val")::tl)  -> sexp_to_val_model_entry tl
  | List _ -> failwith "list fail"
  | Atom _ -> failwith "atom fail"
								     
let sexp_to_model l = List.map sexp_to_model_entry l
			       
let sexp_to_mod_tree t =
  match t with
  | Atom "UNSAT" -> UNSAT
  | Atom "UNKNOWN" -> UNKNOWN
  | List (Atom "SAT"::[List l]) -> SAT (sexp_to_model l)
  | _ -> failwith "Unknown structure"




		  
(* MOD_TREE PRINTER *)

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

let rec fun_conditions_to_string fc = list_to_string (fun (s,t) -> "("^s^" = "^(term_to_string t)^")") andand fc

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
    
let mod_tree_to_string t =
  match t with
  | UNSAT -> "UNSAT"
  | UNKNOWN -> "UNKNOWN"
  | SAT model -> ("SAT (\n"^(model_to_string model)^")")
  	      
let print_mod_tree output_file mod_tree =
  (*val: string -> mod_tree -> unit*)
  let oc = open_out output_file in
  let fft = formatter_of_out_channel oc in
  fprintf fft "%s" (mod_tree_to_string mod_tree);
  fprintf fft "@.%!";
  print_flush ();
  close_out oc
	    


					   
