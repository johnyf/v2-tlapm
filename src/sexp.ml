open Sexplib.Type
open Format
open CCFormat

type sexp_ast = Sexplib.Type.t;;
       
(* READING SEXP FILE *)

let sexp_parser input_file =
  try
    let ic = open_in input_file in
    let t = Sexplib.Sexp.input_sexp ic in

    t
  with
    _ -> failwith ("Unable to parse file "^input_file)

let sexp_printer output_file sexp_ast =
  let oc = open_out output_file in
  Sexplib.Pre_sexp.output_hum oc sexp_ast
