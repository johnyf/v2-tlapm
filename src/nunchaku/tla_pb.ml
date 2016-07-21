open Obligation
open Obligation_formatter
       
(** Definition **)
       
type tla_pb = obligation

(** Printer **)

let fmt_tla_pb = fmt_obligation
