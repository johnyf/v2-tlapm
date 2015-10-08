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


let internal_ds_names =
  object
    inherit [string list] Expr_visitor.visitor as super
    method name acc n = Util.add_missing acc [n]
  end

(* extractor counting suffices in expr tree *)
class expr_suffices_scanner =
object
inherit [int * int] Expr_visitor.visitor as super

method statement (flags, constrs) = function
  | Expr_ds.ST_FORMULA f ->
     let flags = if f.Expr_ds.suffices then flags+1 else flags in
     super#statement (flags, constrs) (Expr_ds.ST_FORMULA f)
  | Expr_ds.ST_SUFFICES f ->
     let flags = if f.Expr_ds.suffices then flags+1 else flags in
     super#statement (flags, constrs+1) (Expr_ds.ST_SUFFICES f)
  | _ as st ->
     super#statement (flags, constrs) st
end

(* extractor counting suffices in sany tree *)
class sany_suffices_scanner =
object
inherit [int * int] visitor as super

method theorem (flags, constrs) = function
  | (THM_ref _) as t -> super#theorem (flags,constrs) t
  | (THM {expr; _ }) as t  ->
     match expr with
     | EA_expr e -> super#theorem (flags, constrs) t
     | EA_assume_prove f ->
        let flags = if f.suffices then flags+1 else flags in
        super#theorem (flags, constrs) t
end

(* test setup *)
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

let test_suffices {sany_context; expr_context; _ } () =
  match sany_context, expr_context with
  | Some sc, Some ec ->
     let sany_counter = new sany_suffices_scanner in
     let expr_counter = new expr_suffices_scanner in
     let (flags,_) = sany_counter#context (0,0) sc in
     let (eflags, constrs) = expr_counter#context (0,0) ec in
     Assert.equal ~msg:("Number of suffices flags in sany DS is different from"
                        ^ " suffices constructors in expr DS!") flags constrs;
     Assert.equal ~msg:("Number of suffices flags in expr DS is different from"
                        ^ " suffices constructors in expr DS!") eflags constrs;
     ()
  | _ ->
     failwith "Could not extract sany/expr context from test record."

let test_xml record =
  Test.make_assert_test
    ~title: ("xml parsing " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_sany record )  )
    )
    (fun () -> ()  )

let test_xml_suffices record =
  Test.make_assert_test
    ~title: ("suffices check after xml parsing " ^ record.filename)
    (fun () -> ())
    (fun () -> exhandler ( test_suffices record )  )
    (fun () -> ()  )

let get_tests records = List.append (List.map test_xml records)
                                    (List.map test_xml_suffices records)
