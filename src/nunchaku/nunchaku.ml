open Format
open CCFormat

type nunchaku_result = Mod.mod_tree

let nunchaku_result_printer = Mod.mod_tree_to_string

let call_nunchaku nun_pb_ast = Sexplib.Type.Atom "UNSAT"

let nunchaku settings obligation =
  let tla_pb_ast = obligation in
  let tla_simple_pb_ast = Simple_obligation.obligation_to_simple_obligation tla_pb_ast in
  let nun_pb_ast = Nunchaku_formatter.simple_obl_to_nun_ast tla_simple_pb_ast in
  let nun_sexp_ast = call_nunchaku nun_pb_ast in
  let nun_mod_ast = Mod.sexp_to_mod_tree nun_sexp_ast in
  nun_mod_ast
                                
(* let print_simple obligations output_file = *)
(*   (\*val: obligation list -> unit*\) *)
(*   let print_obl fft no obl = *)
(*     fprintf fft "Obligation %d:\n%a\n\n" no *)
(* 	    Simple_obligation_formatter.fmt_obligation (Simple_obligation.obligation_to_simple_obligation obl); *)
(*     no+1 *)
(*   in *)
(*   let oc = open_out output_file in *)
(*   let fft = formatter_of_out_channel oc in *)
(*   let for_each_obligation = print_obl fft in *)
(*   ignore(List.fold_left for_each_obligation 1 obligations); *)
(*   fprintf fft "@.%!"; *)
(*   close_out oc *)

	    
(* let print_complex obligations output_file = *)
(*   (\*val: obligation list -> unit*\) *)
(*   let print_obl fft no obl = *)
(*     fprintf fft "Obligation %d:\n%a\n\n" no *)
(* 	    Obligation_formatter.fmt_obligation obl; *)
(*     no+1 *)
(*   in *)
(*   let oc = open_out output_file in *)
(*   let fft = formatter_of_out_channel oc in *)
(*   let for_each_obligation = print_obl fft in *)
(*   ignore(List.fold_left for_each_obligation 1 obligations); *)
(*   fprintf fft "@.%!"; *)
(*   close_out oc *)


(* let print_nunchaku obligations output_file = *)
(*   (\*val: obligation list -> unit*\) *)
(*   let print_obl out no obl = *)
(*     let oc = open_out (output_file ^ "/" ^ (string_of_int no) ^ ".nun") in *)
(*     let fft = formatter_of_out_channel oc in *)
(*     fprintf fft "%a" Simple_obligation_formatter.fmt_nunchaku (Simple_obligation.obligation_to_simple_obligation obl); *)
(*     fprintf fft "@.%!"; *)
(*     close_out oc; *)
(*     no+1 *)
(*   in *)
(*   let for_each_obligation = print_obl output_file in *)
(*   List.fold_left for_each_obligation 1 obligations *)

