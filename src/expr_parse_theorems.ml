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
        if assumes <> [] then super#theorem acc thm
        else
        match
        match_function (tdb acc) prove with
          | Some ("$Case", args) ->
             Printf.printf "found a case?";
             (* TODO: match correctly *)
             super#theorem acc thm
          | Some ("$Pfcase", args) ->
             Printf.printf "found a pfcase?\n";
             (* TODO: match correctly *)
             super#theorem acc thm
          | Some (name, args) ->
             Printf.printf "nope:%s.\n" name;
             super#theorem acc thm
          | None ->
             (
             match prove with
             | E_binder { operator = FMOTA_op_def opd ;
                          operand;
                          bound_symbols; _ } ->
                (
                match dereference_op_def (tdb acc) opd with
                | O_user_defined_op uop  ->
                   (
                   match dereference_user_defined_op (tdb acc) uop with
                   |  { name = "$Pick"; _  } ->
                       (
                       Printf.printf "Pick!";
                       let formula = match operand with
                       | EO_expr expr -> expr
                       | EO_op_arg _ ->
                          failwith ("Don't know what to do with an op_arg as" ^
                                    " parameter for PICK!")
                       in
                       let acc1 = super#theorem acc thm in
                       let statement = ST_PICK { variables = bound_symbols;
                                                 formula; } in
                       let thm = THM { location; level; name; statement; proof;}  in
                       acc1
                       )
                   | _ -> super#theorem acc thm
                   )
                | _ -> super#theorem acc thm
                )
             | _ ->
                Printf.printf "no!\n";
                super#theorem acc thm
             )
        )
     | _ ->
        (* skip other proof step *)
        super#theorem acc thm
     )

method context acc { entries; modules } =
  let acc1 = set_tdb acc (Some entries) in
  super#context acc1 {entries; modules }

end
