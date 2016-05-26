open Sexplib.Type

type term =
  | Var of string
  | App of string * term list
			 
type decision_tree =
  {
    cases: ((string * term) list * term) list;
    else_ : term;
  }

type model_entry =
  | Type of string * string list
  | Const of string * term
  | Fun of string * (string*string) list * decision_tree

type model = model_entry list

type sexp_tree = UNSAT | UNKNOWN | SAT of model | FAIL
							    
let rec tree_to_term t = match t with
  | Atom s -> Var s 	
  | List ((Atom s)::l) -> App (s,(List.map tree_to_term l))
  | _ -> failwith "unparsed term"

let string_to_term s =  Var s
	      
let type_to_model t = match t with
  | ((Atom name)::[List l]) ->
     let rec unroll m = match m with
       | [] -> []
       | (Atom s)::m2 -> s::(unroll m2)
       | _ -> failwith "unroll type var failed"
     in
     Type (name,(unroll l))
  | _ -> failwith "type_to_model failed"

let val_to_model t = match t with
  | ((Atom name)::[value]) -> Const (name,tree_to_term value)
  | _ -> failwith "val_to_model failed"

let fun_to_model t = match t with 
  | ((Atom name)::[List ((Atom "lambda")::(List vars)::[values])]) ->
     let rec unroll_vars v = match v with
       | [] -> []
       | (List ((Atom n)::[Atom t]))::m2 -> (n,t)::(unroll_vars m2)
       | _ -> failwith "unroll fun var failed"
     in
     let rec unroll_conditions c = match c with
       | [] -> []
       | (List [(Atom "=");(Atom v);t])::tl -> (v,tree_to_term t)::(unroll_conditions tl)
       | _ -> failwith "unroll fun conditions failed"
     in
     let match_conditions c = match c with
       | (Atom "and")::tl -> unroll_conditions tl
       | _ -> unroll_conditions c
     in
     let rec unroll_values v = match v with
       | List ((Atom "if")::(List conditions)::(then_)::(tl)::[]) ->
	  let dt = unroll_values tl in
	  {
	    cases = (match_conditions conditions,tree_to_term then_)::dt.cases ;
	    else_ = dt.else_
	  }
       | Atom s             -> {cases = []; else_= tree_to_term v}
       | List ((Atom _)::_) -> {cases = []; else_= tree_to_term v}
       | _ -> failwith "unroll values fun_to_model failed"
     			 
     in
     Fun (name,(unroll_vars vars),(unroll_values values))
  | _ -> failwith "type_to_model failed"
  
let sexplib_tree_to_model t = match t with
  | List ((Atom "type")::tl) -> type_to_model tl 
  | List ((Atom "val")::tl)  -> val_to_model tl 
  | List ((Atom "fun")::tl)  -> fun_to_model tl 
  | List _ -> failwith "list fail"
  | Atom _ -> failwith "atom fail"
								     
let sexplib_list_to_model_list l = List.map sexplib_tree_to_model l
	      
let sexplib_tree_to_sexp_tree t =
  match t with
  | Atom "UNSAT" -> UNSAT
  | Atom "UNKNOWN" -> UNKNOWN
  | List (Atom "SAT"::[List l]) -> SAT (sexplib_list_to_model_list l)
  | _ -> failwith "Unknown structure"

let sexplib_to_sexplib_tree path =
  try
    let ic = open_in path in
    let t = Sexplib.Sexp.input_sexp ic in
    sexplib_tree_to_sexp_tree t
  with
    _ -> FAIL
			      
let rec term_to_string t = match t with
  | Var s -> s
  | App (s,l) -> s^" ("^(list_to_string l)^")"
and
  list_to_string x = match x with
  | [] -> ""
  | [t] -> term_to_string t
  | t::q -> (term_to_string t)^", "^(list_to_string q)
			    
let print_model s =
  let comma = ", " in
  let andand = " && " in
  let rec print_list print_one separator list =
    match list with
    | [] -> ""
    | [x] -> print_one x
    | t::q -> (print_one t)^separator^(print_list print_one separator q)
  in
  let print_type_vars = print_list (fun x -> x) comma
  in
  let print_fun_vars = print_list (fun (s1,s2) -> "("^s1^" : "^s2^")") comma
  in
  let print_fun_conditions = print_list (fun (s,t) -> "("^s^" = "^(term_to_string t)^")") andand
  in
  let rec print_dt dt = match dt.cases with
    | [] -> "\t else : "^(term_to_string dt.else_)
    | (cond_l,then_)::q -> "\t if "^(print_fun_conditions cond_l)^" then : "^(term_to_string then_)^"\n"^(print_dt {cases = q; else_=dt.else_})
  in
  let to_print = match s with
    | Type (name,vars) -> "type "^name^" : "^(print_type_vars vars)
    | Const (name,value) -> "val "^name^" : "^(term_to_string value)
    | Fun (name,vars,dt) -> "fun "^name^" : "^(print_fun_vars vars)^"\n "^(print_dt dt)
  in
  print_string to_print;
  print_newline ();
  ()
    
let print_sexp_tree t =
  let print_ =
  match t with
  | UNSAT -> print_string "UNSAT";
  | UNKNOWN -> print_string "UNKNOWN";
  | SAT l -> print_string "SAT ("; print_newline (); ignore(List.map print_model l) ; print_string ")";
  | FAIL -> print_string "Failed reading model.";
  in
  print_ ;
  print_newline ()
			  
let print_sexp path = print_sexp_tree (sexplib_to_sexplib_tree path)

	     
