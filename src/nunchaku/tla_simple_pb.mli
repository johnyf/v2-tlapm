open Simple_expr_ds

(** Translation **)

val tla_pb_to_tla_simple_pb : Tla_pb.tla_pb -> tla_simple_pb


(** Printer **)

val fmt_tla_simple_pb : Format.formatter -> tla_simple_pb -> unit

val print_tla_simple_pb : string -> tla_simple_pb -> unit
