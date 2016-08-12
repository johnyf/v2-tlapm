open Format

type nun_var = string * string
type nun_operator = Operator of string
type nun_declaration = Declaration of nun_var
type nun_comment = Comment of string
type nun_include = Include of string
type nun_binder = Forall | Exists
type nun_operands = nun_expr
and nun_expr =
  | Op of nun_operator * nun_operands list
  | Binder of nun_binder * nun_var * nun_expr
  | Atom of string
type nun_axiom = Axiom of nun_expr
type nun_goal = Goal of nun_expr

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

let string_of_var (n,t) = n^" : "^t

let rec string_of_expr = function
  | Atom s -> s
  | Binder (Forall, v, e) ->
    let sv = string_of_var v in
    let se = string_of_expr e in
    "forall "^sv^". "^se
  | Binder (Exists, v, e) ->
    let sv = string_of_var v in
    let se = string_of_expr e in
    "exists "^sv^". "^se
  | Op (Operator s, l) ->
    let sl = "" in
    "( "^s^"[ "^sl^"] )"

let print_declaration ppf (Declaration v) = 
  fprintf ppf "val %s. @." (string_of_var v)

let print_axiom ppf (Axiom s) =
  fprintf ppf "axiom : %s. @." (string_of_expr s) 

let print_goal ppf (Goal s) =
  fprintf ppf "goal %s. @." (string_of_expr s)

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


