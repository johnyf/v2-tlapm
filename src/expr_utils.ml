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
  | APSUBST_entry x -> failwith "unpacked apsubst instead of thm"
  | FP_entry x -> failwith "unpacked fp instead of thm"
  | MOD_entry x -> failwith "unpacked mod instead of thm"
  | OPDef_entry x -> failwith "unpacked opdef instead of thm"
  | OPDec_entry x -> failwith "unpacked opdec instead of thm"
  | ASSUME_entry x -> failwith "unpacked assume instead of thm"
  | _ -> failwith (entry_error "thm expected")

let unpack_assume_entry = function
  | ASSUME_entry x -> x
  | _ -> failwith (entry_error "assume")

let unpack_apsubst_entry = function
  | APSUBST_entry x -> x
  | _ -> failwith (entry_error "apsubst")

