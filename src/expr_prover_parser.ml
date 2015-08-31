open Commons
open Expr_ds
open Expr_dereference

let expr_to_prover term_db = function
  | E_op_appl appl  -> (
     match appl.operator with
     | FMOTA_op_def opd -> (
        match dereference_op_def term_db opd with
        | O_user_defined_op uop -> (
           let uopi = dereference_user_defined_op term_db uop in
           match uopi.name, uopi.arity with
           | "SMT", 0 -> Some SMT
           | "LS4", 0 -> Some LS4
           | "Isa", 0 -> Some Isabelle
           | "Zenon", 0 -> Some Zenon
           | _ -> None
        )
        | _ ->  None
     )
     | _ ->  None
  )
  | _ -> None
