open Commons
open Kaputt.Abbreviations
open Sany
open Sany_ds
open Sany_visitor
open Util
open Test_common
open List

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
    inherit [int * int * location list] Expr_visitor.visitor as super

    method statement (flags, constrs, locs) = function
      | Expr_ds.ST_FORMULA f ->
        let flags = if f.Expr_ds.suffices then flags+1 else flags in
        let locs = if f.Expr_ds.suffices then f.Expr_ds.location::locs else locs in
        super#statement (flags, constrs, locs) (Expr_ds.ST_FORMULA f)
      | Expr_ds.ST_SUFFICES f ->
        if (not f.Expr_ds.suffices)
        then failwith "Suffices constructor without flag inside!";
        super#statement (flags+1, constrs+1, f.Expr_ds.location::locs)
          (Expr_ds.ST_SUFFICES f)
      | _ as st ->
        super#statement (flags, constrs, locs) st
  end

let dr_opd entries = function
  | Sany_ds.OPDef_ref r ->
    (
      match List.filter (fun x -> x.uid = r) entries with
      | [] -> failwith "Could not find opdef!"
      | [{reference = Sany_ds.FMOTA_op_def OPDef  od; _}] ->
        od
      | _ -> failwith "Lookup problem!"
    )
  | Sany_ds.OPDef x -> x

let dr_bop entries = function
  | Sany_ds.BOP_ref r ->
    (
      match List.filter (fun x -> x.uid = r) entries with
      | [] -> failwith "Could not find opdef!"
      | [{reference = Sany_ds.FMOTA_op_def OPDef (O_builtin_op (BOP od)); _}] ->
        od
      | _ -> failwith "Lookup problem!"
    )
  | Sany_ds.BOP x -> x


(* extractor counting suffices in sany tree *)
class sany_suffices_scanner =
  object(self)
    inherit [int * int * location list * entry list] visitor as super

    method theorem (flags, constrs, locs, entries) = function
      | (THM_ref _) as t -> super#theorem (flags,constrs,locs,entries) t
      | (THM {expr; location; suffices; _ }) as t  ->
        match expr, location with
        | EA_expr e, Some l -> (
            let flags = if suffices then flags+1 else flags in
            let locs = if suffices then l::locs else locs in
            self#check_suffices entries flags constrs locs suffices t e
          )
        | EA_expr e, None ->
          let flags = if suffices then flags+1 else flags in
          let locs = if suffices then mkDummyLocation::locs else locs in
          self#check_suffices entries flags constrs locs suffices t e
        | EA_assume_prove f, Some l ->
          if (suffices <> f.suffices)
          then failwith "Inconsistent SANY suffices flags!";
          let flags = if f.suffices then flags+1 else flags in
          let locs = if suffices then l::locs else locs in
          super#theorem (flags, constrs, locs,entries) t
        | EA_assume_prove f, None ->
          if (suffices <> f.suffices)
          then failwith "Inconsistent SANY suffices flags!";
          let flags = if f.suffices then flags+1 else flags in
          let locs = if suffices then mkDummyLocation::locs else locs in
          super#theorem (flags, constrs, locs,entries) t

    method context (a,b,c,_) ({entries; modules;} as con) =
      (* add entries to acc for dereferencing *)
      super#context (a,b,c,entries) con

    method private check_suffices entries flags constrs locs suffices t = function
      | Sany_ds.E_op_appl {operator = FMOTA_op_def opd; operands;_} -> (
          match dr_opd entries opd with
          | O_builtin_op op ->
            let name = (dr_bop entries op).name in
            if ((name = "$Suffices") && (not suffices)) then
              failwith "Suffices op there, no flag in theorem";
            if ((name <> "$Suffices") && (suffices)) then
              failwith "Suffices op not there, but flag in theorem";
            super#theorem (flags, constrs, locs,entries) t
          | _ ->
            super#theorem (flags, constrs, locs,entries) t
        )
      | _ ->
        super#theorem (flags, constrs, locs,entries) t

  end

(* *** test setup *** *)
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
    let sacc = (0,0,[],[]) in
    let acc = (0,0,[]) in
    let (flags,_,locs,_) = sany_counter#context sacc sc in
    let (eflags, constrs,elocs) = expr_counter#context acc ec in
(*
     Printf.printf "flags=%d eflags=%d constrs=%d\n"
                   flags eflags constrs;
     List.map (fun l -> Printf.printf "sany loc: %s\n" (format_location l)) locs;
     List.map (fun l -> Printf.printf "expr loc: %s\n" (format_location l)) elocs;
 *)
    Assert.equal ~msg:("counted sany suffices flags and number of location"
                       ^ " must agree." ) flags (length locs)  ;
    Assert.equal ~msg:("counted expr suffices flags and number of constructors"
                       ^ " must agree." ) eflags constrs  ;
    Assert.equal ~msg:("counted expr suffices flags and number of location"
                       ^ " must agree." ) eflags (length elocs);
    Assert.equal ~msg:("Number of suffices flags in sany DS is different from"
                       ^ " suffices constructors in expr DS! "
                       ^ (string_of_int flags) ^ ":" ^ (string_of_int constrs))
      flags constrs;
    Assert.equal ~msg:("Number of suffices flags in expr DS is different from"
                       ^ " suffices constructors in expr DS!"
                       ^ (string_of_int eflags) ^ ":" ^ (string_of_int constrs))
      eflags constrs;

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
    (test_suffices record)
    (* (fun () -> exhandler ( test_suffices record )  ) *)
    (fun () -> ()  )

let get_tests records = List.append (List.map test_xml records)
    (List.map test_xml_suffices records) (**)
