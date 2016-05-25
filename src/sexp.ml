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

type sexp_tree = UNSAT | UNKNOWN | SAT of model (* TODO : add a failure case when Nunchaku failed e.g. obl 11 *)

let string_to_term s = Var s 	(* TODO : interprete types *)
					    
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
  | ((Atom name)::[Atom value]) -> Const (name,string_to_term value)
  | _ -> failwith "val_to_model failed"

let fun_to_model t = match t with (* FIXME : fails with obligation 7 in case no if *)
  | ((Atom name)::[List ((Atom "lambda")::(List vars)::[List values])]) ->
     let rec unroll_vars v = match v with
       | [] -> []
       | (List ((Atom n)::[Atom t]))::m2 -> (n,t)::(unroll_vars m2)
       | _ -> failwith "unroll fun var failed"
     in
     let rec unroll_conditions c = match c with
       | [] -> []
       | (List [(Atom "=");(Atom v);(Atom t)])::tl -> (v,string_to_term t)::(unroll_conditions tl)
       | _ -> failwith "unroll fun conditions failed"
     in
     let match_conditions c = match c with
       | (Atom "and")::tl -> unroll_conditions tl
       | _ -> unroll_conditions c
     in
     let rec unroll_values v = match v with
       | [Atom a] -> {cases = []; else_= string_to_term a}
       | ((Atom "if")::(List conditions)::(Atom s_then)::(Atom s_else)::[]) ->
	  {
	    cases = [match_conditions conditions, string_to_term s_then];
	    else_ = string_to_term s_else
	  }
       | ((Atom "if")::(List conditions)::(Atom s_then)::(List l)::[]) ->
	  let dt = unroll_values l in
	  {
	    cases = (match_conditions conditions,string_to_term s_then)::dt.cases ;
	    else_ = dt.else_
	  }
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
  let ic = open_in path in
  let t = Sexplib.Sexp.input_sexp ic in
  sexplib_tree_to_sexp_tree t

let rec term_to_string t = match t with
  | Var s -> s
  | App (s,l) -> s^" ("^(list_to_string l)^")"
and
  list_to_string x = match x with
  | [] -> ""
  | [t] -> term_to_string t
  | t::q -> (term_to_string t)^(list_to_string q)
			    
let print_model s =
  let rec print_list print_one list =
    match list with
    | [] -> ""
    | [x] -> print_one x
    | t::q -> (print_one t)^", "^(print_list print_one q)
  in
  let print_type_vars = print_list (fun x -> x)
  in
  let print_fun_vars = print_list (fun (s1,s2) -> "("^s1^" : "^s2^")")
  in
  let print_fun_conditions = print_list (fun (s,t) -> "("^s^" = "^(term_to_string t)^")")
  in
  let rec print_dt dt = match dt.cases with
    | [] -> "\t else : "^(term_to_string dt.else_)
    | (cond_l,then_)::q -> "\t if "^(print_fun_conditions cond_l)^", then : "^(term_to_string then_)^"\n"^(print_dt {cases = q; else_=dt.else_})
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
  in
  print_ ;
  print_newline ()
			  
let print_sexp s = print_sexp_tree (sexplib_to_sexplib_tree s)

	     
