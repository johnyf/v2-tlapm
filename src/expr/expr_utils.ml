open List
open Expr_ds
open Expr_builtins
open Expr_visitor
open Util

module Unpack = struct
  let entry_error str =
    "Tried to unpack a " ^ str ^ " entry, but it isn't one."

  let fp_entry = function
    | FP_entry x -> x
    | _ -> failwith (entry_error "fp")

  let builtin_op_entry = function
    | BOP_entry x -> x
    | _ -> failwith (entry_error "bop")

  let user_defined_op_entry = function
    | UOP_entry x -> x
    | _ -> failwith (entry_error "uop")

  let module_instance_entry = function
    | MI_entry x -> x
    | _ -> failwith (entry_error "mi")

  let mod_entry = function
    | MOD_entry x -> x
    | _ -> failwith (entry_error "mod")

  let opdecl_entry = function
    | OPDec_entry x -> x
    | _ -> failwith (entry_error "opdec")

  let thm_entry = function
    | THM_entry x -> x
    | _ -> failwith (entry_error "thm")

  let mi_entry = function
    | MI_entry x -> x
    | _ -> failwith (entry_error "mi")

  let uop_entry = function
    | UOP_entry x -> x
    | _ -> failwith (entry_error "uop")

  let bop_entry = function
    | BOP_entry x -> x
    | _ -> failwith (entry_error "bop")

  let theorem_def_entry = function
    | TDef_entry x -> x
    | _ -> failwith (entry_error "thm_def")

  let assume_def_entry = function
    | ADef_entry x -> x
    | _ -> failwith (entry_error "asm_def")

  let assume_entry = function
    | ASSUME_entry x -> x
    | _ -> failwith (entry_error "assume")

end

let location_of_expr = function
  | E_at { location; level }
  | E_decimal { location; level; _ }
  | E_label { location; level; _ }
  | E_let_in { location; level; _ }
  | E_numeral { location; level; _ }
  | E_op_appl { location; level; _ }
  | E_string { location; level; _ }
  | E_subst_in { location; level; _ }
  | E_binder {location; level; _ }
  | E_fp_subst_in {location; level; _ } -> location

let level_of_expr = function
  | E_at { location; level; _ }
  | E_decimal { location; level; _ }
  | E_label { location; level; _ }
  | E_let_in { location; level; _ }
  | E_numeral { location; level; _ }
  | E_op_appl { location; level; _ }
  | E_string { location; level; _ }
  | E_subst_in { location; level; _ }
  | E_binder {location; level; _ }
  | E_fp_subst_in {location; level; _ } -> level

let level_of_expr_or_op_arg = function
  | EO_expr e -> level_of_expr e
  | EO_op_arg {level; _} -> level

let location_of_expr_or_op_arg = function
  | EO_expr e -> location_of_expr e
  | EO_op_arg {location; _} -> location

(** wraps an expression into an assume-prove with empty assumptions *)
let assume_prove_from_expr suffices expr =  {
  location = location_of_expr expr;
  level = level_of_expr expr;
  assumes = [];
  new_symbols = [];
  prove = expr;
  suffices;
  boxed = false; (* TODO: check if this is true *)
}

let level_of_node = function
  | N_expr e -> level_of_expr e
  | N_ap_subst_in ap -> ap.level
  | N_assume_prove ap -> ap.level

