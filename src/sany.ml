(* Copyright (C) 2014 MSR-INRIA
 *
 * SANY expressions
 * This file should be broken into xml utilities, sany ds and sany export
 * Remark: the parser does not chek for additional data between nodes, e.g. 
 * adding newlines will result in parsing failures.
 *
 * Author: TL
 *)

open Commons
open Xmlm

type node =
  | N_ap_subst_in of ap_subst_in
  | N_assume_prove of assume_prove
  | N_def_step of def_step
  | N_expr of expr
  | N_op_arg of op_arg
  | N_instance of instance
  | N_new_symb of new_symb
  | N_proof of proof
  | N_formal_param of formal_param
  | N_module of mule
  | N_op_decl of op_decl
  | N_op_def of op_def
  | N_assume of assume
  | N_theorem of theorem
  | N_use_or_hide of use_or_hide

and expr =
  | E_at of at
  | E_decimal of decimal
  | E_label of label
  | E_let_in of let_in
  | E_numeral of numeral
  | E_op_appl of op_appl
  | E_string of strng
  | E_subst_in of subst_in

and expr_or_op_arg =
  | EO_expr of expr
  | EO_op_arg of op_arg

and expr_or_assume_prove =
  | EA_expr of expr
  | EA_assume_prove of assume_prove

and  new_symb_or_expr_or_assume_prove =
  | NEA_new_symb of new_symb
  | NEA_expr of expr
  | NEA_assume_prove of assume_prove

and ap_subst_in = {
  location          : location;
  level             : level;
  substs            : subst list;
  body              : node
}

and subst_in = {
  location          : location;
  level             : level;
  substs            : subst list;
  body              : expr
}

and instance = {
  location          : location;
  level             : level;
  name              : string;
  substs            : subst list;
  params            : formal_param list
}

and subst = {
  location          : location;
  level             : level;
  op                : op_decl;
  expr              : expr_or_op_arg
}

and assume =
| ASSUME_ref of int
| ASSUME of assume_

and assume_ = {
  location          : location;
  level             : level;
  expr              : expr
}

and theorem =
| THM_ref of int
| THM of theorem_

and theorem_ = {
  location          : location;
  level             : level;
  expr              : expr_or_assume_prove;
  proof             : proof;
  suffices          : bool
}

and assume_prove = {
  location          : location;
  level             : level;
  assumes           : new_symb_or_expr_or_assume_prove list;
  prove             : expr;
  suffices          : bool;
  boxed             : bool
}

and new_symb = {
  location          : location;
  level             : level;
  op_decl           : op_decl;
  set               : expr
}

and op_def =
  | OPDef_ref of int
  | OPDef of op_def_
    
and op_def_ =
  | O_module_instance of module_instance
  | O_user_defined_op of user_defined_op
  | O_builtin_op of builtin_op

and module_instance =
  | MI_ref of int
  | MI of module_instance_

and module_instance_ = {
  location          : location;
  level             : level;
  name              : string
}

and user_defined_op =
  | UOP_ref of int
  | UOP of user_defined_op_

and user_defined_op_ = {
  location          : location;
  level             : level;
  name              : string;
  arity             : int;
  body              : expr;
  params            : (formal_param * bool (*is leibniz*)) list;
  recursive         : bool
}

and builtin_op =
  | BOP_ref of int
  | BOP of builtin_op_

and builtin_op_ = {
  location          : location;
  level             : level;
  name              : string;
  arity             : int;
  params            : (formal_param * bool (*is leibniz*)) list
}

and op_arg = {
  location          : location;
  level             : level;
  name              : string;
  arity             : int
}

and formal_param =
   | FP_ref of int
   | FP of formal_param_

and formal_param_ = {
     location          : location;
     level             : level; 
     name              : string;
     arity             : int
}

and op_decl =
   | OPD_ref of int
   | OPD of op_decl_
      
and op_decl_ = {
  location          : location;
  level             : level;
  name              : string;
  arity             : int;
  kind              : op_decl_kind
}

and proof =
  | P_omitted of omitted
  | P_obvious of obvious
  | P_by of by
  | P_steps of steps

and omitted = {
  location          : location;
  level             : level;
}

and obvious = {
  location          : location;
  level             : level;
}

and expr_or_module_or_module_instance =
  | EMM_expr of expr
  | EMM_module of mule
  | EMM_module_instance of module_instance

and user_defined_op_or_module_instance_or_theorem_or_assume =
  | UMTA_user_definewd_op of user_defined_op
  | UMTA_module_instance of module_instance
  | UMTA_theorem of theorem
  | UMTA_assume of assume

and by = {
  location          : location;
  level             : level;
  facts             : expr_or_module_or_module_instance list;
  defs              : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only              : bool
}

and steps = {
  location          : location;
  level             : level;
  steps             : step list
}

and step =
  | S_def_step of def_step
  | S_use_or_hide of use_or_hide
  | S_instance of instance
  | S_theorem of theorem

and def_step = {
  location          : location;
  level             : level;
  defs              : op_def list
}

and use_or_hide = {
  location          : location;
  level             : level;
  facts             : expr_or_module_or_module_instance list;
  defs              : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only              : bool;
  hide              : bool
}

and at = {
  location          : location;
  level             : level;
  except            : op_appl;
  except_component  : op_appl
}

and decimal = {
  location          : location;
  level             : level;
  mantissa          : int;
  exponent          : int
}

and label = {
  location          : location;
  level             : level;
  name              : string;
  arity             : int;
  body              : expr_or_assume_prove;
  params            : formal_param list
}

and op_def_or_theorem_or_assume =
  | OTA_op_def of op_def
  | OTA_theorem of theorem
  | OTA_assume of assume

and let_in = {
  location          : location;
  level             : level;
  body              : expr;
  op_defs           : op_def_or_theorem_or_assume list
}

and numeral = {
  location          : location;
  level             : level;
  value             : int
}

and strng = {
  location          : location;
  level             : level;
  value             : string
}

and formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume =
  | FMOTA_formal_param of formal_param
  | FMOTA_module of mule
  | FMOTA_op_decl of op_decl
  | FMOTA_op_def of op_def
  | FMOTA_theorem of theorem
  | FMOTA_assume of assume

and op_appl = {
  location          : location;
  level             : level;
  operator          : formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume;
  operands          : expr_or_op_arg list;
  bound_symbols     : bound_symbol list
}

and bound_symbol =
  | B_unbounded_bound_symbol of unbounded_bound_symbol
  | B_bound_bound_symbol of bounded_bound_symbol

and unbounded_bound_symbol = {
  param             : formal_param;
  tuple             : bool
}

and bounded_bound_symbol = {
  params            : formal_param list;
  tuple             : bool;
  domain            : expr
}

(* modules *)
and mule =
  | MOD_ref of int
  | MOD of mule_
    
and mule_ = {
  name              : string;
  location          : location;
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assume list ;
  theorems          : theorem list ;
}

  

(* this map represents the context, mapping UIDs to formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume *)
module ContextMap =  Map.Make(struct type t = int let compare : int -> int -> int = compare end) (* Map.Make(Integer) *)


let formatSignal (i : Xmlm.signal) = match i with
  | `El_start tg -> "<" ^ (snd (fst tg)) ^ ">"
  | `El_end -> "</>"
  | `Dtd t -> "dtd"
  | `Data str -> "Data:" ^ str 

(*
 * if context is given, then we check both tg and tg^"Ref"
 * and if there is Ref, we get the item from the context.
 * MRI: that only works if the referenced item has been parsed first: current plan is parse with ref, then insert refs
 * We need to remember to use mk functions for term sharing
 * TODO in SANY, move the context element to be appended first in the tree (before, location, name, etc.)
 * TODO: After resolving refs, the data structure may be cyclic. No dereferencing for now?
 *)
(**
  con : context
  i   : xml input stream
  tgs_funs : list of pairs string * fun

  processes children (left to right) of the current node. the list tgs_fun contains pairs of functions (p, f), where the 
  p acts as a guard to apply f, i.e. if (p name) is true, we apply f to the child. processing stops when the first 
  unrecognized name is encountered.

  Remark: get_children_choice itself only looks at the tags, they have to be consumed by the functions applied.
*)
let rec get_children_choice ?context:(con=None) i tgs_funs =
  let filter_by_name tag = List.filter (fun x ->
    let p = (fst x) in let name = snd (fst tag) in p name) tgs_funs in
  match (peek i) with 
  | `El_start tag -> (
    match (filter_by_name tag) with (* check if one of the names applies       *)
    | (_, f) :: _ ->              (* find the corresponding function         *)
      let child =  f i in
      child :: (get_children_choice ~context:con i tgs_funs)
    | [] -> []                    (* if no predicate matches, return nothing *)
  )
  | `El_end -> []
  | _ -> failwith ("Illegal XML element, expecting child nodes but got " ^ (formatSignal (peek i)))

(** this is a reduction of get_children_choice to 0 or 1 occurrences  *)
let get_optchild_choice ?context:(con=None) i tgs_funs =
  match get_children_choice ~context:con i tgs_funs with
  | (_x1::_x2::_xs) as l ->
    failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
		 " but got " ^ (string_of_int (List.length l)))
  | list -> list
  
(** this is a reduction of get_children_choice to exatly 1 occurrences  *)
let get_child_choice ?context:(con=None) i tgs_funs =
  match get_children_choice ~context:con i tgs_funs with
  | [x] -> x
  | l   -> failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
			" but got " ^ (string_of_int (List.length l)))
    
(** processes children (left to right) of the current node. return a list of children having the name tg and
   maps the function f on each of them. processing stops after the first child not named tg  
*)
let get_children ?context:(con=None) i tg f =
  get_children_choice ~context:con i [((=) tg,f)]    (* the guard is just equality to tg *)

(** this is a reduction of get_children to 0 or 1 occurrences  *)
let get_optchild ?context:(con=None) i tg f =
  match get_children_choice ~context:con i [((=) tg,f)] with
  | (_x1::_x2::_xs) as l ->
    failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
		 " but got " ^ (string_of_int (List.length l)))
  | list -> list

let get_child ?context:(con=None) i tg f =
  let chldn = get_children ~context:con i tg f in
  assert (List.length chldn = 1);
  List.hd chldn

    

let open_tag i tg = let signal = (input i) in match signal  with
  | `El_start tag when (snd (fst tag) = tg) -> ()
  | `El_start tag -> failwith ("Illegal XML start tag: " ^ (snd(fst tag)) ^ ". expected: " ^ tg ^ ".")
  | `El_end -> failwith ("Illegal tag encountered: expecting start tag for: " ^ tg ^
				", but got end tag.")
  | _ -> failwith ("Illegal XML element, expecting open tag: " ^ (formatSignal signal))

let close_tag i tg = match (input i) with
  | `El_end -> () (* can we check that we expect the end to be </tg>? *)
  | `El_start d -> failwith ("Illegal tag encountered: expecting end tag for: " ^ tg ^
				", but got start tag for: " ^ (snd(fst d)))
  | _ -> failwith ("Illegal XML element, expecting close tag: " ^ (formatSignal (peek i)))

(* expects a node named tg_par and returns the all the children with tag tg_chdren. 
   remark: tag is closed afterwards, cannot process any remaining children
  *)
let get_children_in ?context:(con=None) i tg_par tg_chdrn f =
  open_tag i tg_par;

let ret = get_children ~context:con i tg_chdrn f in
  close_tag i tg_par;
  ret

let get_child_in ?context:(con=None) i tg_par tg_chd f =
  let chldn = get_children_in ~context:con i tg_par tg_chd f in
  assert (List.length chldn = 1);
  List.hd chldn

let get_data_in i tg f =
  open_tag i tg;
  let ret = f i in
  close_tag i tg;
  ret

let read_int i = match (input i) with
  | `Data d -> int_of_string d
  | _ -> failwith "expected data element"

let read_string i = match (input i) with
  | `Data d -> d
  | _ -> failwith "expected data element"


let init_context_map ls = ContextMap.empty

let mkLevel i = match i with
  | 0 -> ConstantLevel
  | 1 -> VariableLevel
  | 2 -> ActionLevel
  | 3 -> TemporalLevel
  | _ -> Errors.bug ("XML Parser error: unknown level " ^ (string_of_int i) ^ " (expected 0-3).")
  
(** Parses a location node, returning a record with line and column *)
let read_location i =
  open_tag i "location";
  open_tag i "column";
  let cb = get_data_in i "begin" read_int in
  let ce = get_data_in i "end" read_int in
  close_tag i "column";
  open_tag i "line";
  let lb = get_data_in i "begin" read_int in
  let le = get_data_in i "end" read_int in
  close_tag i "line";
  let fname = get_data_in i "filename" read_string in
  close_tag i "location";
  { column = {rbegin = cb; rend = ce};
    line = {rbegin = lb; rend = le};
    filename = fname
  }

  
(** Parses the FormalParamNode within context/entry *)
let read_formal_param i =
  open_tag i "FormalParamNode";
  let loc = read_location i in
  let level = get_data_in i "level" read_int in
  let un = get_data_in i "uniquename" read_string in
  let ar = get_data_in i "arity" read_int in
  close_tag i "FormalParamNode";
  FP {
    location = loc;
    level = mkLevel level;
    arity = ar;
    name = un;
  }

(** Parses the OpArgNode *)
let read_oparg i =
  open_tag i "OpArgNode";
  let loc = read_location i in
  let level = get_data_in i "level" read_int in
  let un = get_data_in i "uniquename" read_string in
  let ar = get_data_in i "arity" read_int in
  close_tag i "OpArgNode";
  {
    location = loc;
    level = mkLevel level;
    arity = ar;
    name = un;
  }

    
(** gets the UID number of the reference node "name" *)    
let read_ref i name f =
  open_tag i name;
  open_tag i "UID";
  let str = read_int i in
  close_tag i "UID";
  close_tag i name;
  f str

(** reads one of reference arguments *)
let read_opref i =
    let name = match (peek i) with 
     | `El_start ((_, name),_ ) -> name
     | signal -> failwith "We expect a symbol opening tag of an operator reference but got " ^ (formatSignal signal)
   in 
    let opref = match name with
      | "FormalParamNodeRef"    -> read_ref i name (fun x -> FMOTA_formal_param (FP_ref x) )
      | "ModuleNodeRef"         -> read_ref i name (fun x -> FMOTA_module (MOD_ref x) )
      | "OpDeclNodeRef"         -> read_ref i name (fun x -> FMOTA_op_decl (OPD_ref x) )
      | "ModuleInstanceKindRef" -> read_ref i name (fun x -> FMOTA_op_def (OPDef_ref x) )
      | "UserDefinedOpKindRef"  -> read_ref i name (fun x -> FMOTA_op_def (OPDef_ref x) )
      | "BuiltInKindRef"        -> read_ref i name (fun x -> FMOTA_op_def (OPDef_ref x) )
      | "TheoremNodeRef"        -> read_ref i name (fun x -> FMOTA_theorem (THM_ref x) )
      | "AssumeNodeRef"         -> read_ref i name (fun x -> FMOTA_assume (ASSUME_ref x) )
      | _ -> failwith ("Found tag " ^ name ^ " but we need an operator reference (FormalParamNodeRef, "^
	               "ModuleNodeRef, OpDeclNodeRef, OpDefNodeRef, TheoremNodeRef, AssumeNodeRef)")
    in
    opref



(** handles the leibnizparam tag *)
let read_param i =
  open_tag i "leibnizparam";
  let wrap_fp x = FP_ref x in
  let fpref = read_ref i "FormalParamNodeRef" wrap_fp in
  let nonempty l = (List.length l) <> 0 in
  let read_leibnizflag x = open_tag i "leibniz";close_tag i "leibniz"; true in
  let is_leibniz = nonempty (get_children i "leibniz" read_leibnizflag)  in
  let ret = (fpref, is_leibniz) in
  close_tag i "leibnizparam";
  ret
    
let read_params i =
  get_children_in i "params" "leibnizparam" read_param
  
 
(** Parses the BuiltinKind within context/entry *)
let read_builtin_kind i =
  open_tag i "BuiltInKind";
  let loc = read_location i in
  let level = get_data_in i "level" read_int in
  let un = get_data_in i "uniquename" read_string in
  let ar = get_data_in i "arity" read_int in
  let params = get_children i "params" read_params in
  close_tag i "BuiltInKind";
  BOP {
    location = loc;
    arity = ar;
    name = un;
    params = List.flatten params;
    level = mkLevel level;
  }

let read_module_instance_kind i = assert false

(* --- expressions parsing is mutually recursive (e.g. OpApplNode Expr) ) --- *)
let rec read_expr i =
  let name = match (peek i) with 
     | `El_start ((_, name),_ ) -> name
     | _ -> failwith "We expect symbol opening tag in an entry."
   in 
   let expr = match name with 
     | "AtNode"      -> E_at (read_at i)
     | "DecimalNode" -> E_decimal (read_decimal i)
     | "LabelNode"   -> E_label (read_label i)
     | "LetInNode"   -> E_let_in (read_let i)
     | "NumeralNode" -> E_numeral (read_numeral i)
     | "OpApplNode"  -> E_op_appl (read_opappl i)
     | "StringNode"  -> E_string (read_stringnode i)
     | "SubstInNode" -> E_subst_in (read_substinnode i)
     | _ -> failwith ("Unexpected node start tag for expression " ^ name ^
		      ", expected one of AtNode, DecimalNode, LabelNode, "^
		      " LetInNode, NumeralNode, OpApplNode, StringNode or SubstInNode." )
   in
   expr

 
and read_at i           = assert false
and read_decimal i 	= assert false
and read_label i	= assert false  
and read_let i	  	= assert false
and read_numeral i	= assert false  
and read_opappl i	=
  open_tag i "OpApplNode";
  let loc = read_location i in
  let level = get_data_in i "level" read_int in
  open_tag i "operator";
  let opref = read_opref i in
  close_tag i "operator";
  open_tag i "operands";
  let operands = get_children_choice i [
    ((=) "OpArgNode", (fun i -> EO_op_arg (read_oparg i)));
    ((fun x -> true), (fun i -> EO_expr (read_expr i)) )
  ]
  in
  close_tag i "operands";
  let bound_symbols = match (peek i) with
    | `El_start ((_,name), _) ->
      open_tag i "boundSymbols";
      let bs = get_children_choice i [
	((=) "unbound", (fun i -> B_unbounded_bound_symbol (read_unbounded_param i)) );
	((=) "bound",   (fun i -> B_bound_bound_symbol (read_bounded_param i)) )
      ] in
      close_tag i "boundSymbols";
      bs
    | _ -> []
  in
  let ret =  {
    location = loc;
    level = mkLevel level;
    operator = opref;
    operands = operands;
    bound_symbols = bound_symbols;
  }
  in
  close_tag i "OpApplNode";
  ret
  
and read_stringnode i	= assert false  
and read_substinnode i	= assert false
and read_tuple i = match (peek i) with
  | `El_start ((_,"tuple"), _) ->
  (* if tag is present, consume tag and return true*)
  open_tag i "tuple";
  close_tag i "tuple";
  true
  | _ ->  (* otherwise return false *)
  false
  
and read_bounded_param i =
  open_tag i "bound";
  let params = get_children i "FormalParamNode" read_formal_param in
  let tuple = read_tuple i in
  let domain = read_expr i in
  let ret =  {
    params = params;
    tuple = tuple;
    domain = domain;
  } in
  close_tag i "bound";
  ret
   
and read_unbounded_param i =
  open_tag i "unbound";
  let params = read_formal_param i in
  let tuple = read_tuple i in
  let ret =  {
    param = params;
    tuple = tuple;
  } in
  close_tag i "unbound";
  ret
(*
and read_expr_or_oparg i =
  let name = match (peek i) with 
    | `El_start ((_, name),_ ) -> name
    | _ -> failwith "We expect symbol opening tag in an entry."
  in
  let ret = match name with
    | "OpArgNode" -> EO_op_arg (read_oparg i)
    | _           -> EO_expr (read_expr i) (* all the others are handled by expr *)
  in
  ret
*)
(* --- end of mutual recursive expression parsing --- *)

(** consumes the recursive flag and returns true *)
let read_recursive i =
  open_tag i "recursive";
  close_tag i "recursive";
  true
  
(** reads the definition of a user defined operator within context/entry *)
let read_userdefinedop_kind i  =
  open_tag i "UserDefinedOpKind";
  let loc = read_location i in
  let level = get_data_in i "level" read_int in
  let un = get_data_in i "uniquename" read_string in
  let ar = get_data_in i "arity" read_int in
  open_tag i "body";
  let body = read_expr i in
  close_tag i "body";
  let params = List.flatten
    (get_optchild i "params" read_params) in
  let recursive = List.length (get_optchild i "recursive" read_recursive) > 0 in
  let ret = UOP {
    location = loc;
    arity = ar;
    name = un;
    level = mkLevel level;
    body = body;
    params = params;
    recursive = recursive;
  } in
  close_tag i "UserDefinedOpKind";
  ret
    
let rec read_entry i =
   let _ = open_tag i "entry" in 
   let uid = get_data_in i "UID" read_int in
   let name = match (peek i) with 
     | `El_start ((_, name),_ ) -> name
     | _ -> failwith "We expect symbol opening tag in an entry."
   in 
   let symbol = match name with 
     | "FormalParamNode"    -> FMOTA_formal_param (read_formal_param i)
     (* Operator definition nodes: ModuleInstanceKind, UserDefinedOpKind, BuiltinKind *)
     | "UserDefinedOpKind"  -> FMOTA_op_def (OPDef (O_user_defined_op (read_userdefinedop_kind i))) 
     | "ModuleInstanceKind" -> FMOTA_op_def (OPDef (O_module_instance (read_module_instance_kind i))) (* TODO *)
     | "BuiltInKind"        -> FMOTA_op_def (OPDef (O_builtin_op (read_builtin_kind i)))
     | _ -> failwith ("Unhandled context node " ^ name)
   in let _ = close_tag i "entry";
   in  (uid, symbol)


  
let read_name i = assert false
let read_op_decl i = assert false
let read_op_def i = assert false
let read_assume i = assert false
let read_theorem i = assert false

let read_module con i =
  open_tag i "ModuleNode";
  let loc = get_child i "location" read_location in
  (* we need to read the context first and pass it for the symbols (thm, const, etc.*)
  (*  let con = Some (init_context_map (get_children_in i "context" "entry" read_entry)) in *)
  let name = get_data_in i "uniquename" read_string in
  (* print_string name; *)
  let constants = get_children_in ~context:con i "constants" "OpDeclNode" read_op_decl in
  let variables = get_children_in ~context:con i "variables" "OpDeclNode" read_op_decl in
  let definitions = get_children_in ~context:con i "definitions" "OpDefNode" read_op_def in
  let assumptions = get_children_in ~context:con i "assumptions" "AssumeNode" read_assume in
  let theorems = get_children_in ~context:con i "theorems" "TheoremNode" read_theorem in
  let ret = MOD {
    location     = loc;
    name         = name;
    constants    = constants;
    variables    = variables;
    definitions  = definitions;
    assumptions  = assumptions;
    theorems     = theorems;
  } in
  close_tag i "ModuleNode";
  ret


let read_modules i =
  open_tag i "modules";
  let con = (init_context_map (get_children_in i "context" "entry" read_entry) ) in 
  let ret = get_children i "ModuleNode" (read_module (Some con)) in
  close_tag i "modules";
  ret

let read_header i =
  input i (* first symbol is dtd *)

  (*while true do match (input i) with
  | `Data d -> print_string ("data: " ^ d ^ "\n")
  | `Dtd d -> print_string "dtd\n"
  | `El_start d -> print_string ("start: " ^ (snd(fst d)) ^ "\n")
  | `El_end -> print_string "end\n"
  done*)

let import_xml ic =
  let i = Xmlm.make_input (`Channel ic) in
  let _ = read_header i in
  read_modules i
  (*while not (Xmlm.eoi i) do match (Xmlm.input i) with
  done;
  assert false*)

