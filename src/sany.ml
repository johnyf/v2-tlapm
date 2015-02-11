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
open Sany_ds
open Xml_utils

let init_context_map ls = ContextMap.empty

let mkLevel i = match i with
  | 0 -> ConstantLevel
  | 1 -> VariableLevel
  | 2 -> ActionLevel
  | 3 -> TemporalLevel
  | _ -> Errors.bug ("XML Parser error: unknown level " ^
			(string_of_int i) ^ " (expected 0-3).")
  
(** Parses a location node, returning a record with line and column *)
let read_location i : location =
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
let read_formal_param i : formal_param =
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
let read_oparg i : op_arg =
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
and read_tuple i : bool = match (peek i) with
  | `El_start ((_,"tuple"), _) ->
  (* if tag is present, consume tag and return true*)
  open_tag i "tuple";
  close_tag i "tuple";
  true
  | _ ->  (* otherwise return false *)
  false
  
and read_bounded_param i : bounded_bound_symbol =
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
   
and read_unbounded_param i : unbounded_bound_symbol =
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

