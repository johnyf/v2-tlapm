open Expr_ds
open Expr_map
open Expr_utils
open Expr_prover_parser
open Expr_dereference

type 'a ptacc = term_db option * 'a

let get_ptacc_tdb (db,_) = db
let tdb macc =
  match get_ptacc_tdb (get_acc macc) with
  | Some db -> db
  | _ -> failwith "Accumulator does not contain the term database!"

let set_tdb macc db =
  let (_, rest) = get_acc macc in
  set_acc macc (db, rest)

class ['a] expr_parse_theorems =
object(self)
inherit ['a ptacc] expr_map as super

method theorem acc thm = match thm with
  | THM_ref i -> super#theorem acc thm
  | THM { location; level; name; statement; proof; } ->
     (
     match statement with
     | ST_FORMULA { location; level; new_symbols;
                    assumes; prove; suffices; boxed; } ->
        (
        match assumes, prove, match_function (tdb acc) prove with
        | _::_, _, _ -> super#theorem acc thm
        | _, _, Some ("$Pfcase", args) ->
           (* Printf.printf "Case!"; *)
           (* recurse on subterms *)
           let acc1 = super#theorem acc thm in
           let extract = self#get_macc_extractor in
           let THM {location; level; name; statement; proof } =
             extract#theorem acc1 in
           (* create new theorem and update accumulator *)
           let statement = match args with
           | [EO_expr expr] -> ST_CASE expr
           | [EO_op_arg _] ->
              failwith "Don't know what to do with op arg passed to case step!"
           | _ ->
              failwith "Step case operator expects exactly one argument!"
           in
           let thm = THM {location; level; name; statement; proof } in
           set_anyexpr acc1 (Any_theorem thm)
        | _, E_binder { operator = FMOTA_op_def opd ;
                          operand;
                          bound_symbols; _ }, None ->
           (
           match dereference_op_def (tdb acc) opd with
           | O_builtin_op { name = "$Pick"; _  } ->
              (
              (* Printf.printf "Pick!"; *)
              (* recurse on subterms *)
              let acc1 = super#theorem acc thm in
              let extract = self#get_macc_extractor in
              let THM {location; level; name; statement; proof } =
                extract#theorem acc1 in
              (* change formula to pick version *)
              let formula = match operand with
              | EO_expr expr -> expr
              | EO_op_arg _ ->
                 failwith ("Don't know what to do with an op_arg as" ^
                           " parameter for PICK!")
              in

              let statement = ST_PICK { variables = bound_symbols;
                                        formula; } in
              (* create new theorem and update accumulator *)
              let thm = THM { location; level; name; statement; proof;}  in
              set_anyexpr acc1 (Any_theorem thm)
              )
           | _ -> super#theorem acc thm
           )
        | _ ->
           super#theorem acc thm
        )
     | _ ->
        (* skip other proof step *)
        super#theorem acc thm
     )

method context acc { entries; modules } =
  let acc1 = set_tdb acc (Some entries) in
  super#context acc1 {entries; modules }

end
