open Expr_ds
open Expr_utils
open List

let find_entry unpack entries i =
  let elem = try
      assoc i entries
    with
    | Not_found ->
      failwith ("Could not find entry for reference " ^
                (string_of_int i) ^ "!")
  in
  unpack elem

let dereference_user_defined_op term_db = function
  | UOP d -> d
  | UOP_ref r ->
    let opd = find_entry unpack_opdef_entry term_db r in
    match opd with
    | O_user_defined_op (UOP op) -> op
    | _ -> failwith ("The id " ^ (string_of_int r) ^
                     " does refer to a user defined operator!")

let dereference_formal_param term_db = function
  | FP fp -> fp
  | FP_ref i ->
    find_entry unpack_fp_entry term_db i

let dereference_op_decl term_db = function
  | OPD opd -> opd
  | OPD_ref x ->
    find_entry unpack_opdecl_entry term_db x

let dereference_user_defined_op term_db = function
  | UOP d -> d
  | UOP_ref x ->
    match find_entry unpack_opdef_entry term_db x with
    | O_user_defined_op (UOP op) -> op
    | O_user_defined_op _ ->
      failwith "Self-reference in term db!"
    | _ ->
      let str = Printf.sprintf
          "The id %d does not refer to a user defined operator!" x
      in
      failwith str

let dereference_module_instance term_db = function
  | MI d -> d
  | MI_ref x ->
    match find_entry unpack_opdef_entry term_db x with
    | O_module_instance (MI m) -> m
    | O_module_instance _ ->
      failwith "Self-reference in term db!"
    | _ ->
      let str = Printf.sprintf
          "The id %d does not refer to a user defined operator!" x
      in
      failwith str

let dereference_theorem term_db =  function
  | THM t -> t
  | THM_ref x ->
    find_entry unpack_thm_entry term_db x

let dereference_assume term_db =  function
  | ASSUME t -> t
  | ASSUME_ref x ->
    find_entry unpack_assume_entry term_db x

let dereference_module term_db =  function
  | MOD t -> t
  | MOD_ref x ->
    find_entry unpack_mod_entry term_db x

let compare_modulo_deref_formal_param term_db ?cmp:(cmp=(=)) f1 f2 =
  let fp1i = dereference_formal_param term_db f1 in
  let fp2i = dereference_formal_param term_db f2 in
  cmp fp1i fp2i
