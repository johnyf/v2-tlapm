open Format

type nun_declaration = Declaration of string * string
type nun_axiom = Axiom of string
type nun_goal = Goal of string
type nun_comment = Comment of string
type nun_include = Include of string

type nun_statement = nun_goal * nun_declaration list * nun_axiom list * nun_comment list * nun_include list

let newline_formatter channel =
  fprintf channel "@,";
  ()

let sepatation_formatter channel =
  fprintf channel "%s@, " "\n# ---------------------------------------------\n";
  ()
    
let rec ppf_fold_with ppf f = function
  | [x] -> f ppf x
  | x::xs ->
     f ppf x;
     newline_formatter ppf;
     ppf_fold_with ppf f xs
  | [] -> ()

let print_declaration ppf (Declaration (n,t)) = 
  fprintf ppf "val %s : %s. @." n t
  
let print_axiom ppf (Axiom s) =
  fprintf ppf "axiom : %s. @." s 
  
let print_goal ppf (Goal s) =
  fprintf ppf "goal %s. @." s 
  
let print_comment ppf (Comment s) =
  fprintf ppf "# %s @." s
	  
let print_include ppf (Include s) =
  fprintf ppf "include \"%s\". @." s
	  
let print_statement ppf (go, dec, ax, com, inc) =
  ppf_fold_with ppf print_comment com;
  sepatation_formatter ppf; 
  newline_formatter ppf;
  ppf_fold_with ppf print_include inc;
  sepatation_formatter ppf; 
  ppf_fold_with ppf print_declaration dec;
  sepatation_formatter ppf; 
  ppf_fold_with ppf print_axiom ax;
  sepatation_formatter ppf; 
  print_goal ppf go
												   

