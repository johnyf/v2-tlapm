open Sany
open Tlapm_args
open Obligation
open Extract_obligations
open Format
open Nunchaku
open Simple_obligation_formatter
open Expr_simple
open Expr_substitution
open Simple_obligation
open Simple_expr_prover_parser
open Sexp
       
(** Creates the command line string used to invoke the sany parser *)
let java_cmd offline search_path input_files = "java -jar lib/sany.jar" ^
  (if !offline then " -o " else "") ^ (* add offline flag, if neccessary *)
    (if (List.length search_path > 0) then
       " -I " ^ (String.concat " -I " search_path) (* add include directories*)
     else "")
    ^ " " ^ (String.concat " " input_files) (* add input file *)

let tla_to_xml tla_filename xml_filename =
  let tla_to_xml = "sh tla2xml.sh -o -I ./library/ "^tla_filename^" > "^xml_filename in
  ignore(Sys.command tla_to_xml)

let xml_to_obl xml_filename output_file=
  let channel = open_in xml_filename in
  (* load sany xml ast from file *)
  let sany_context = Sany.import_xml channel in
  (* extract builtins from file *)
  let sany_builtins =
    Sany_builtin_extractor.extract_from_context sany_context in
  (* convert sany ast to internal ast (expr_ds) *)
  let exprds =
    Sany_expr.convert_context ~builtins:sany_builtins sany_context in
  (* replace definitions of name LAMBDA by lambda constructors *)
  let fixed_lambda =
    Expr_correct_lambda.correct_lambda_context exprds in
  (* make language elements represented as builtin operators explicit *)
  let fixed_theorems =
    Expr_parse_theorems.expr_parse_theorems_context fixed_lambda in
  (* extract obligations *)
  let obligations =
    Extract_obligations.extract_obligations_context fixed_theorems in
  (* print obligations to stdout *)
  (*
     ignore(
         List.fold_left (fun no obl ->
                         fprintf std_formatter "Obligation %d:\n%a\n\n" no
                                 Obligation_formatter.fmt_obligation obl;
                         no+1
                        ) 1 obligations
       )
   *)
  (* call nunckaku *)
  (* print_complex obligations "nun/complex.txt"; *)
  print_simple obligations output_file;
  obligations

let obl_to_nun obligations dir =
  (* Directory in which the .nun files will be created. One file per obligation. *)
  (* The directory needs to exist, otherwise it won't work. *)
  (* TODO Add a command to create the directory if it doesn't exist. *)
  print_nunchaku obligations dir

let nun_to_sexp nun_file sexp_file =
  let nunk = "nunchaku -o sexp "^nun_file^" > "^sexp_file in
  ignore(Sys.command nunk)
		  
let print_nun k =
  let sk = "echo \"\n----- OBLIGATION "^(string_of_int k)^": -----\n\"" in
  ignore(Sys.command sk) ;
  let nunk = "nunchaku nun/nun/"^(string_of_int k)^".nun" in
  ignore(Sys.command nunk)

let rec call_nun k = match k with
  | 0 -> ();
  | _ -> call_nun (k-1) ;
	 nun_to_sexp ("nun/nun/"^(string_of_int k)^".nun") ("nun/sexp/"^(string_of_int k)^".sexp")

let sexp_to_mod sexp_file mod_file =
  Sexp.print_sexp sexp_file mod_file
  		     
let rec convert_to_mod k = match k with
  | 0 -> ();
  | _ -> convert_to_mod (k-1);
	 sexp_to_mod ("nun/sexp/"^(string_of_int k)^".sexp") ("nun/mod/"^(string_of_int k)^".mod")

let print_all obligations n =
  let f obl k =
    let sk = "echo \"\n----- OBLIGATION "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    fprintf std_formatter "%a" Obligation_formatter.fmt_obligation obl ;
    print_newline ();
    let sk = "echo \"\n----- NUNCHAKU "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/nun/"^(string_of_int k)^".nun" in
    ignore(Sys.command nunk);
    let sk = "echo \"\n----- SEXP "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/sexp/"^(string_of_int k)^".sexp" in
    ignore(Sys.command nunk);
    let sk = "echo \"\n----- MOD "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/mod/"^(string_of_int k)^".mod" in
    ignore(Sys.command nunk);
       
  in
  let rec map_bis f l n = match l with
    | [] -> []
    | hd::tl -> let t = (f hd n) in t::(map_bis f tl (n+1))
  in
  ignore (map_bis f obligations 1)
		     
let init () =
  (* argument handling TODO: rewrite *)
  match  Array.length Sys.argv with

  | 2 ->
     let filename = Sys.argv.(1) in
     let tla_filename = ("nun/tla/"^filename^".tla") in
     let xml_filename = ("nun/xml/"^filename^".xml") in
     tla_to_xml tla_filename xml_filename;
     let obligations = xml_to_obl xml_filename "nun/obligations.txt" in
     let n = obl_to_nun obligations "nun/nun" in
     call_nun (n-1) ;
     convert_to_mod (n-1) ;
     print_all obligations (n-1);
     print_newline ()
	   
  | 4 when Sys.argv.(1)="sexp2mod" ->
     sexp_to_mod Sys.argv.(2) Sys.argv.(3)
	   
  | 4 when Sys.argv.(1)="tla2xml" ->
     tla_to_xml Sys.argv.(2) Sys.argv.(3)

  | 4 when Sys.argv.(1)="xml2obl" ->
     let xml_filename = Sys.argv.(2) in
     let target = Sys.argv.(3) in
     ignore(xml_to_obl xml_filename target);
     Printf.eprintf "TLAPM wrote obligations in %s.\n" target;
  
  | _ ->
     Printf.eprintf "Syntax: ./tlapm.byte file_name\n";
     ()
     
;;


init ();;
