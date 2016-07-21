open Commons
open Simple_expr_ds

type tla_simple_pb = {
    (* actual obligation, without expansion *)
    goal : simple_assume_prove;
    
    (* the term database *)
    term_db : simple_term_db;
}
			   
val tla_pb_to_tla_simple_pb : Tla_pb.tla_pb -> tla_simple_pb
                                                                                 
(** Printer **)

val fmt_tla_simple_pb : Format.formatter -> tla_simple_pb -> unit
