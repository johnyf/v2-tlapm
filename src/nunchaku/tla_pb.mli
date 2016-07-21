(** Definition **)
       
type tla_pb = Obligation.obligation
                
(** Printer **)

val fmt_tla_pb : Format.formatter -> tla_pb -> unit
