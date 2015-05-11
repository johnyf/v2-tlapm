open Kaputt.Abbreviations
open Sany
open Sany_ds
open Sany_visitor
open Util
open Test_common


(** extracts all names *)
let name_visitor =
  object
    inherit [string list] visitor as super
    method name acc n = Util.add_missing acc [n]
  end

(** extracts all references *)
let ref_visitor =
  object
    inherit [int list] visitor as super
    method reference acc i = Util.add_missing acc [i]
  end

(** extracts dependencies between references *)
let dependency_visitor =
  object
    inherit [(int * int) list] visitor as super
    method entry acc { uid; reference } =
      let deps = ref_visitor#fmota [] reference in
      let pairs = List.map (fun x -> (uid, x)) deps in
      List.append acc pairs
  end

let internal_ds_names =
  object
    inherit [string list] Expr_visitor.visitor as super
    method name acc n = Util.add_missing acc [n]
  end

let test_sany record () =
  let channel = open_in record.filename in
  let tree = exhandler (fun () -> import_xml channel) in
  close_in channel;
  let sany_names = List.sort compare (name_visitor#context [] tree) in
  (* Printf.printf "%s\n" (Util.mkString (fun x->x) sany_names); *)
  let builtins = Sany_builtin_extractor.extract_from_context tree in
  let etree = Sany_expr.convert_context tree ~builtins:builtins in
  let internal_names = List.sort compare (internal_ds_names#context [] etree) in
  (* Printf.printf "%s\n" (Util.mkString (fun x->x) internal_names); *)
  Assert.equal
    ~msg:("Names extracted from SANY XML are different " ^
            "from the ones in the internal data-structrues!")
    sany_names internal_names;
  (* update test result record *)
  record.sany_context <- Some tree;
  record.expr_context <- Some etree;
  tree

let test_xml record =
  Test.make_assert_test
    ~title: ("xml parsing " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_sany record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_xml records
