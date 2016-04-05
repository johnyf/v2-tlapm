open Format
open Simple_obligation
       
let nunchaku obligations output_file =
(*val: obligation list -> unit*)
  let print_obl fft no obl =
    fprintf fft "Obligation %d:\n%a\n\n" no
	    Simple_obligation_formatter.fmt_obligation (obligation_to_simple_obligation obl);
    no+1
  in
  let oc = open_out output_file in
  let fft = formatter_of_out_channel oc in
  let for_each_obligation = print_obl fft in
  ignore(List.fold_left for_each_obligation 1 obligations);
  close_out oc

  let no_nunchaku obligations output_file =
    (*val: obligation list -> unit*)
  let print_obl fft no obl =
    fprintf fft "Obligation %d:\n%a\n\n" no
	    Obligation_formatter.fmt_obligation obl;
    no+1
  in
  let oc = open_out output_file in
  let fft = formatter_of_out_channel oc in
  let for_each_obligation = print_obl fft in
  ignore(List.fold_left for_each_obligation 1 obligations);
  close_out oc

