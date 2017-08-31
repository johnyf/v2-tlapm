open Expr_ds
open Expr_utils
open List

module Deref = struct
  let find_entry unpack entries i =
    let elem = try
        assoc i entries
      with
      | Not_found ->
        failwith ("Could not find entry for reference " ^
                  (string_of_int i) ^ "!")
    in
    unpack elem

  let builtin_op term_db = function
    | BOP_ref r ->
      find_entry Unpack.builtin_op_entry term_db r

  let user_defined_op term_db = function
    | UOP_ref r ->
      find_entry Unpack.user_defined_op_entry term_db r

  let formal_param term_db = function
    | FP_ref i ->
      find_entry Unpack.fp_entry term_db i

  let op_decl term_db = function
    | OPD_ref x ->
      find_entry Unpack.opdecl_entry term_db x

  let module_instance term_db = function
    | MI_ref x ->
      find_entry Unpack.module_instance_entry term_db x

  let theorem_def term_db = function
    | TDef_ref x ->
      find_entry Unpack.theorem_def_entry term_db x

  let assume_def term_db = function
    | ADef_ref x ->
      find_entry Unpack.assume_def_entry term_db x

  let theorem term_db =  function
    | THM_ref x ->
      find_entry Unpack.thm_entry term_db x

  let assume term_db =  function
    | ASSUME_ref x ->
      find_entry Unpack.assume_entry term_db x

  let mule term_db =  function
    | MOD_ref x ->
      find_entry Unpack.mod_entry term_db x

  let compare_modulo_deref_formal_param term_db ?cmp:(cmp=(=)) f1 f2 =
    let fp1i = formal_param term_db f1 in
    let fp2i = formal_param term_db f2 in
    cmp fp1i fp2i
end
