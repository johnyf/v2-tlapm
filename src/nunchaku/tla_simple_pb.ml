open Simple_expr_ds
open Simple_expr
open Tla_pb

(** Definition **)

type tla_simple_pb = {
  (* actual obligation, without expansion *)
  goal : simple_assume_prove;

  (* the term database *)
  term_db : simple_term_db;
}


(** Translation **)

let tla_pb_to_tla_simple_pb (tla_pb:tla_pb) =
  let (stermdb, ap) = parse_expr tla_pb.term_db tla_pb.goal in
  {goal = ap; term_db = stermdb;}



(** Printer **)

let fmt_tla_simple_pb pp { goal; term_db; } =
  let acc = (pp, term_db, false, Simple_expr_formatter.Expression, 0) in
  ignore(Simple_expr_formatter.expr_formatter#assume_prove acc goal);
  Format.print_flush ()

let print_tla_simple_pb file obl =
  let oc = open_out file in
  let fft = Format.formatter_of_out_channel oc in
  Format.fprintf fft "%a" fmt_tla_simple_pb obl;
  Format.fprintf fft "@.%!";
  close_out oc
