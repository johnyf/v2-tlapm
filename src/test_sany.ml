open Kaputt.Abbreviations
open Sany
open Sany_ds
open Sany_visitor

let exhandler f =
  try
    Printexc.record_backtrace true;
    let ret = f () in
    Printexc.record_backtrace false;
    ret
  with
    x ->
      Printf.printf "Exception: %s\n" (Printexc.to_string x);
      Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
      raise x
(** extracts all names *)
let name_visitor = object
  inherit [string list] visitor as super
  method name acc n = Util.add_missing acc [n]
end

(** extracts all references *)
let ref_visitor = object
  inherit [int list] visitor as super
  method reference acc i = Util.add_missing acc [i]
end

(** extracts dependencies between references *)
let dependency_visitor = object
  inherit [(int * int) list] visitor as super
  method entry acc { uid; reference } =
    let deps = ref_visitor#fmota [] reference in
    let pairs = List.map (fun x -> (uid, x)) deps in
    List.append acc pairs
end
  

let test_xml filename =
  Test.make_simple_test
    ~title: ("xml parsing " ^ filename)
    (fun () ->
      Assert.no_raise ~msg:"Unexpected exception raised." (fun () ->
	let channel = open_in filename in
	let tree = exhandler (fun () -> import_xml channel)
	in
	close_in channel;
	let acc = name_visitor#context [] tree in
	Printf.printf "%s\n" (Util.mkString (fun x->x) acc);
	let deps = dependency_visitor#context [] tree in
	Printf.printf "Dependency pairs: %s\n" (Util.mkString ~front:"digraph out {" ~middle:"\n" ~back:"}" (Util.fmtPair ~front:"" ~middle:" -> " ~back:";" string_of_int string_of_int) deps);
	let ordering = exhandler (fun () -> Util.find_ordering deps) in
	Printf.printf "Found an ordering: %s\n" (Util.mkString string_of_int ordering);
	tree
      )
    )

let addpath = (fun (str : string) -> "test/resources/" ^ str ^ ".xml")

let files = List.map addpath [
  "empty";
  "UserDefOp";
(*  "cyclic"; (* cyclic dependencies -- skipped *) *)
  "tuples";
  "Choose";
  "at" ; 
  "expr" ; 
(*  "Euclid" ; (* cyclic dependencies -- skipped *) *)
 (*   "pharos" ; (* commented out, takes 10s to load *)  *)
  "instanceA" ; 
(*  "exec" ; (* cyclic dependencies -- skipped *) *)
(*  "priming_stephan" ; (* cyclic dependencies -- skipped *) *)
  "withsubmodule" ; 
  "OneBit" ; 
]
  
let get_tests = List.map test_xml files

