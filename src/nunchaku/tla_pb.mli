(** Definition **)
       
type tla_pb = Obligation.obligation
                
(** Printer **)

val fmt_tla_pb : Format.formatter -> tla_pb -> unit

val print_tla_pb : string -> tla_pb -> unit
