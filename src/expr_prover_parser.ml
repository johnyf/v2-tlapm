open Commons
open Expr_ds
open Expr_dereference

let match_constant term_db = function
  | E_op_appl appl  -> (
     match appl.operator with
     | FMOTA_op_def opd -> (
        match dereference_op_def term_db opd with
        | O_user_defined_op uop ->
           let uopi = dereference_user_defined_op term_db uop in
           Some uopi.name
        | O_builtin_op bop ->
           Some bop.name
        | _ ->  None
     )
     | _ ->  None
  )
  | _ -> None

let expr_to_prover term_db expr =
  match match_constant term_db expr with
  | Some "SMT" -> Some SMT
  | Some "LS4" -> Some LS4
  | Some "Isa" -> Some Isabelle
  | Some "Zenon" -> Some Zenon
  | _ -> None
