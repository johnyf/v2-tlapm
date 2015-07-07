open Expr_ds

let entry_error str =
  "Tried to unpack a " ^ str ^ " entry, but it isn't one."

let unpack_fp_entry = function
  | FP_entry x -> x
  | _ -> failwith (entry_error "fp")

let unpack_mod_entry = function
  | MOD_entry x -> x
  | _ -> failwith (entry_error "mod")

let unpack_opdecl_entry = function
  | OPDec_entry x -> x
  | _ -> failwith (entry_error "opdec")

let unpack_opdef_entry = function
  | OPDef_entry x -> x
  | _ -> failwith (entry_error "opdef")

let unpack_thm_entry = function
  | THM_entry x -> x
  | APSUBST_entry x -> failwith "apsubst"
  | FP_entry x -> failwith "fp"
  | MOD_entry x -> failwith "mod"
  | OPDef_entry x -> failwith "opdef"
  | OPDec_entry x -> failwith "opdec"
  | ASSUME_entry x -> failwith "assume"   (* *)
  | _ -> failwith (entry_error "thm")

let unpack_assume_entry = function
  | ASSUME_entry x -> x
  | _ -> failwith (entry_error "assume")

let unpack_apsubst_entry = function
  | APSUBST_entry x -> x
  | _ -> failwith (entry_error "apsubst")
