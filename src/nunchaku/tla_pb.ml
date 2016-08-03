open Obligation
open Obligation_formatter

(** Definition **)

type tla_pb = obligation

(** Printer **)

let fmt_tla_pb = fmt_obligation

let print_tla_pb file obl =
  let oc = open_out file in
  let fft = Format.formatter_of_out_channel oc in
  Format.fprintf fft "%a" fmt_tla_pb obl;
  Format.fprintf fft "@.%!";
  close_out oc

