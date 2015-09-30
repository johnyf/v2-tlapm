open Expr_ds
open Expr_map
open Expr_utils
open Expr_prover_parser

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
  | THM { location; level; name; statement; proof; } as thmi ->
     (
     match statement with
     | ST_FORMULA { location; level; new_symbols;
                    assumes; prove; suffices; boxed; } ->
        (
        if assumes <> [] then super#theorem acc thm
        else
        match match_function (tdb acc) prove with
        | Some ("$Pick", args) ->
           (* TODO: match correctly *)
           super#theorem acc thm
        | Some ("$Case", args) ->
           (* TODO: match correctly *)
           super#theorem acc thm
        | Some ("$Pfcase", args) ->
           (* TODO: match correctly *)
           super#theorem acc thm
        | _ ->
           super#theorem acc thm
        )
     | _ ->
        super#theorem acc thm
     )

method context acc { entries; modules } =
  let acc1 = set_tdb acc (Some entries) in
  super#context acc1 {entries; modules }

end
