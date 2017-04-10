open Commons
open Kaputt.Abbreviations
open Sany
open Sany_ds
open Sany_visitor
open Util
open Test_common
open List

module StringSet = CCSet.Make(String)

(** extracts all names *)
let name_visitor =
  object
    inherit [StringSet.t] visitor as super
    method name acc n = StringSet.add n acc
  end

let internal_ds_names =
  object
    inherit [StringSet.t] Expr_visitor.visitor as super
    method name acc n = StringSet.add n acc
  end

(* extractor counting suffices in expr tree *)
class expr_suffices_scanner =
  object
    inherit [int * int * location list] Expr_visitor.visitor as super

    method statement (flags, constrs, locs) = function
      | Expr_ds.ST_FORMULA (Expr_ds.N_assume_prove f) ->
        let flags = if f.Expr_ds.suffices then flags+1 else flags in
        let locs =
          if f.Expr_ds.suffices
          then f.Expr_ds.location::locs
          else locs
        in
        super#statement (flags, constrs, locs)
          (Expr_ds.ST_FORMULA (Expr_ds.N_assume_prove f))
      | Expr_ds.ST_SUFFICES (Expr_ds.N_assume_prove f) ->
        if (not f.Expr_ds.suffices)
        then failwith "Suffices constructor without flag inside!";
        super#statement (flags+1, constrs+1, f.Expr_ds.location::locs)
          (Expr_ds.ST_SUFFICES (Expr_ds.N_assume_prove f))
      (* TODO check ab_subst_in and expr *)
      | _ as st ->
        super#statement (flags, constrs, locs) st
  end


let dr_bop entries = function
  | Sany_ds.BOP_ref r ->
    (
      match List.filter (fun x -> x.uid = r) entries with
      | [] -> failwith "Could not find opdef!"
      | [{reference = Sany_ds.E_builtin_op od; _}] ->
        Some od
      | _ ->
        None
    )
  | Sany_ds.BOP x ->
    Some x

let dr_opd entries = function
  | Sany_ds.OPDef_ref r ->
    dr_bop entries (BOP_ref r)
  | Sany_ds.OPDef (O_builtin_op bop) ->
    dr_bop entries bop

(* extractor counting suffices in sany tree *)
class sany_suffices_scanner =
  object(self)
    inherit [int * int * location list * entry list] visitor as super

    method private e_or_ap_in_node =
      let rec aux = function
        | N_expr e -> EA_expr e
        | N_assume_prove ap -> EA_assume_prove ap
        | N_ap_subst_in {body; _} ->
          aux body
      in function
        | EA_ap_subst_in ap -> aux ap.body
        | ea -> ea

    method theorem (flags, constrs, locs, entries) = function
      | (THM_ref _) as t -> super#theorem (flags,constrs,locs,entries) t
      | (THM {expr; location; suffices; _ }) as t  ->
        let l = match location with
          | None -> mkDummyLocation
          | Some l -> l
        in
        match self#e_or_ap_in_node expr with
        | EA_expr e ->
            let flags = if suffices then flags+1 else flags in
            let locs = if suffices then l::locs else locs in
            self#check_suffices entries flags constrs locs suffices t e
        | EA_assume_prove f ->
          if (suffices <> f.suffices)
          then failwith "Inconsistent SANY suffices flags!";
          let flags = if f.suffices then flags+1 else flags in
          let locs = if suffices then l::locs else locs in
          super#theorem (flags, constrs, locs,entries) t
        | EA_ap_subst_in aps ->
          failwith "Implementation error!"
            

    method context (a,b,c,_) ({entries; modules;} as con) =
      (* add entries to acc for dereferencing *)
      super#context (a,b,c,entries) con

    method private check_suffices entries flags constrs locs suffices t = function
      | Sany_ds.E_op_appl {operator = FMOTA_op_def opd; operands;_} -> (
          match dr_opd entries opd with
          | Some op ->
            if ((op.name = "$Suffices") && (not suffices)) then
              failwith "Suffices op there, no flag in theorem";
            if ((op.name <> "$Suffices") && (suffices)) then
              failwith "Suffices op not there, but flag in theorem";
            super#theorem (flags, constrs, locs,entries) t
          | None ->
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
  let emptyset = StringSet.empty in
  let sany_ns = (name_visitor#context emptyset tree) in
  let sany_names = StringSet.filter ((<>) "LAMBDA") sany_ns
  in
  (* Printf.printf "%s\n" (Util.mkString (fun x->x) sany_names); *)
  (*  let builtins = Sany_builtin_extractor.extract_from_context tree in *)
  let etree = Sany_expr.convert_context tree ~builtins:[] in
  let internal_ns = internal_ds_names#context emptyset etree in
  let internal_names = StringSet.filter ((<>) "LAMBDA") internal_ns in
  let diff1 = StringSet.diff sany_names internal_names in
  let diff2 = StringSet.diff internal_names sany_names in
  let pp_sset = StringSet.pp ~start:"{" ~stop:"}" ~sep:", " CCFormat.string in
  let msg1 =
    CCFormat.sprintf "@[<v>SANY and EXPR names must agree@,S-E:%a@,@]"
      pp_sset diff1
  in
  let msg2 =
    CCFormat.sprintf "@[<v>SANY and EXPR names must agree@,E-S:%a@,@]"
      pp_sset diff2
  in
  Assert.equal ~msg:msg1 (StringSet.is_empty diff1) true;
  Assert.equal ~msg:msg2 (StringSet.is_empty diff2) true;
  (* update test result record *)
  record.sany_context <- Some tree;
  record.expr_context <- Some etree;
  tree
(*
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

*)
let test_xml record =
  Test.make_assert_test
    ~title: ("xml parsing " ^ record.filename)
    (fun () -> ())
(*    (fun () ->
       Assert.no_raise ~msg:"Unexpected exception raised."*)
         (fun () -> exhandler ( test_sany record )  )
         (*    )*)
    (fun _ -> ()  )

(*
let test_xml_suffices record =
  Test.make_assert_test
    ~title: ("suffices check after xml parsing " ^ record.filename)
    (fun () -> ())
    (test_suffices record)
    (* (fun () -> exhandler ( test_suffices record )  ) *)
    (fun () -> ()  )
*)
let get_tests records =
  List.append (List.map test_xml records) []
    (* (List.map test_xml_suffices records) *)
