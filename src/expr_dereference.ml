open Expr_ds
open Expr_utils
open List

let find_entry unpack con i =
  let entries = con.entries in
  let elem = assoc i entries in
  unpack elem


let dereference_user_defined_op context = function
  | UOP d -> d
  | UOP_ref r ->
     let opd = find_entry unpack_opdef_entry context r in
     match opd with
     | O_user_defined_op (UOP op) -> op
     | _ -> failwith ("The id " ^ (string_of_int r) ^
                      " does refer to a user defined operator!")
let dereference_formal_param context = function
  | FP fp -> fp
  | FP_ref i ->
     find_entry unpack_fp_entry context i

let dereference_op_decl context = function
    | OPD opd -> opd
    | OPD_ref x ->
       find_entry unpack_opdecl_entry context x

let dereference_op_def context = function
  | OPDef d -> d
  | OPDef_ref x ->
     find_entry unpack_opdef_entry context x
let dereference_theorem context =  function
  | THM t -> t
  | THM_ref x ->
     find_entry unpack_thm_entry context x
