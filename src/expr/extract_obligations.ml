open Printf
open List
open Commons
open Expr_ds
open Expr_builtins
open Expr_utils
open Expr_dereference
open Expr_formatter
open Expr_visitor
open Obligation
open Expr_prover_parser
open Util

(* customized failwith for obligation extraction *)
let failwith_msg loc msg =
  let e = sprintf "Error extracting obligation at %s: %s"
      (format_location loc) msg in
  failwith e

(* used to track the nesting level throughout a proof *)
type nesting =
  | Module
  | InProof of int

(* used to track the currently visible objects *)
type current_context = {
  (* the current goal *)
  goal : node option;

  (* the facts which should be used as assumptions *)
  usable_facts : node list;

  (* the expanded definitions *)
  expanded_defs : op_def list;

  (* the term database *)
  term_db : term_db;

  (* visible theorems etc.  not sure if we need that *)
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assume list ;
  theorems          : theorem list ;

  (* the own step has to be treated differently, so we keep it seperate *)
  thm_statements : (theorem_def * node list) list;
}

let emptyCurrentContext term_db = {
  goal = None;
  usable_facts = [];
  expanded_defs = [];
  term_db;
  constants = [];
  variables = [];
  definitions = [];
  assumptions = [];
  theorems = [];
  thm_statements = [];
}

(*
let print_cc formatter {goal; usable_facts; expanded_defs; term_db;
                        constants; variables; assumptions; theorems; thm_statements; } =
  fprintf formatter "Goal: ";
  (
    match goal with
    | Some g ->
       fprintf formatter "%a" fmt_assume_prove g;
       ()
    | None ->
       fprintf formatter "(None)";
       ()
  );
  ()
 *)

(* type representing if a proof step creates a context for a subproof (inner),
   the global proof (outer) or both *)
type step_context_type =
    NoContext |
    Inner of current_context |
    Outer of current_context |
    OuterInner of current_context * current_context

type ofail = ObligationFail of string * location

(* the accumulator type - containing an open parameter for derived classes *)
type 'a eoacc =
    EOAcc of current_context list * obligation list * ofail list
             * nesting * string *'a

(* extractors for the constituents of the accumulator *)
let get_cc (EOAcc (c, _, _, _, _, _)) = c
let get_obligations (EOAcc (_, o, _, _, _, _)) = o
let get_failed_obligations (EOAcc (_, _, f, _, _, _)) = f
let get_nesting (EOAcc (_, _, _, n, _, _)) = n
let get_root_module (EOAcc (_, _, _, _, m, _)) = m

(* accumulator updates *)
let update_cc (EOAcc (_,o,f,n,m,a)) cc = EOAcc (cc,o,f,n,m,a)
let update_obligations (EOAcc (cc,_,f,n,m,a)) o = EOAcc (cc,o,f,n,m,a)
let update_nesting (EOAcc (cc,o,f,_,m,a)) n = EOAcc (cc,o,f,n,m,a)
let update_root_module (EOAcc (cc,o,f,n,_,a)) m = EOAcc (cc,o,f,n,m,a)

let enqueue_failure (EOAcc (cc,o,f,n,m,a)) nf = (EOAcc (cc,o,nf::f,n,m,a))

(* convenience function adding one obligation to the accumulator *)
let enqueue_obligation acc newobs =
  let obs = get_obligations acc in
  let obs_ids = map (fun x -> x.id) obs in
  match flat_map (fun x -> if mem x.id obs_ids then [x.id] else []) newobs with
  | [] ->
    update_obligations acc (append obs newobs)
  | ids ->
    let msg = Format.asprintf "Duplicate obligation ids: %a"
        (fmt_list Format.pp_print_int) ids
    in
    failwith msg

(* convenience functions managing the nesting *)
let increase_nesting acc =
  match get_nesting acc with
  | Module -> update_nesting acc (InProof 1)
  | InProof n -> update_nesting acc (InProof (n+1))

let decrease_nesting acc =
  match get_nesting acc with
  | Module -> failwith "Cannot decrease nesting level below module!"
  | InProof 1 -> update_nesting acc Module
  | InProof n when n > 1 -> update_nesting acc (InProof (n-1))
  | InProof n (* when n <= 0 *) ->
    failwith "Nesting may not go below 1!"

let cc_empty acc =
  match get_cc acc with
  | [] -> true
  | _ -> false

let cc_stack_size acc =
  length (get_cc acc)

let cc_push cc acc =
  (*  Printf.printf "Push! %d\n" (cc_stack_size acc); *)
  update_cc acc (cc::(get_cc acc))

let cc_pop acc =
  (* Printf.printf "Pop! %d\n" (cc_stack_size acc); *)
  match get_cc acc with
  | [] -> failwith "Trying to pop an empty stack!"
  | x::xs -> (x, update_cc acc xs)

let cc_peek acc =
  match get_cc acc with
  | [] -> failwith "Trying peek on an empty stack!"
  | x::_ -> x

let cc_replace cc acc =
  let (_, acc0) = cc_pop acc in
  cc_push cc acc0


(* generates an obligation id based on the current list of obligations *)
let generate_id acc =
  (length (get_obligations acc)) + 1

(* extracts prover tags from by list *)
let rec split_provers term_db = function
  | [] -> ([],[])
  (* prover pragmas are expression, everything else is ignored *)
  | (EMM_expr x) as element::xs -> (
      let (provers, exprs) = split_provers term_db xs in
      match expr_to_prover term_db x with
      | Some prover ->
        (prover :: provers, exprs)
      | None ->
        (provers,element::exprs)
    )
  | x::xs ->
    let (provers, exprs) = split_provers term_db xs in
    (provers,x::exprs)

(* replaces assume / theorem definition references by the actual statements *)
let expand_theorem_bodies term_db visible_defs facts =
  let rec aux term_db =
    List.fold_left (fun acc -> function
        | EMM_expr (E_op_appl { location;
                                operator = FMOTA_op_def ((O_thm_def td) as o);
                                operands; _ }) ->
          let tdi = Deref.theorem_def term_db td in
          if (not (List.mem o visible_defs)) then
            let msg = CCFormat.sprintf "Theorem def %s not visible!" tdi.name in
            failwith_msg location msg
          else
            tdi.body::acc
        | EMM_expr (E_op_appl { location;
                                operator = FMOTA_op_def ((O_assume_def ad) as o);
                                operands; _ }) ->
          let adi = Deref.assume_def term_db ad in
          if (not (List.mem o visible_defs)) then
            let msg = CCFormat.sprintf "Assume def %s not visible!" adi.name in
            failwith_msg location msg
          else
            (N_expr adi.body)::acc
        | EMM_expr exp ->
          (N_expr exp) :: acc
        | EMM_module m ->
          failwith "don't know what to do with BY M, where M is a module"
        | EMM_module_instance m ->
          failwith "don't know what to do with BY M, where M is a module instance"
      ) []
  in
  (* during fold we prepended, reverse lists to preserve order *)
  aux term_db facts |> List.rev


(* the actual visitor subclass *)
class ['a] extract_obligations =
  object(self)
    inherit ['a eoacc] visitor

    method assume acc a =
      match get_nesting acc with
      | Module ->
        let cc = cc_peek acc in
        let assumptions = a :: cc.assumptions in
        let ai = Deref.assume cc.term_db a in
        let definitions = CCOpt.map_or ~default:cc.definitions
            (fun x -> (O_assume_def x)::cc.definitions) ai.definition in
        cc_replace { cc with assumptions; definitions } acc
      | InProof n ->
        let msg = CCFormat.sprintf "assume at nesting %d!" n in
        failwith msg;

    method op_decl acc opdecl =
      match get_nesting acc with
      | Module ->
        let cc = cc_peek acc in
        let decl_instance = Deref.op_decl cc.term_db opdecl in
        let ccnew = match decl_instance.kind with
          | ConstantDecl -> { cc with constants = opdecl :: cc.constants }
          | VariableDecl -> { cc with variables = opdecl :: cc.variables }
          | _ -> cc in
        cc_replace ccnew acc
      | _ ->
        acc

    method op_def acc opdef =
      let cc = cc_peek acc in
      let definitions = opdef :: cc.definitions in
      cc_replace { cc with definitions } acc

    method private parse_by_def term_db = function
      | UMTA_user_defined_op uop ->
        [O_user_defined_op uop]
      | UMTA_module_instance mi ->
        let mii = Deref.module_instance term_db mi in
        failwith_msg mii.location
          "don't know what to do with module instance in BY DEF!"
      | UMTA_theorem_def thm ->
        [O_thm_def thm]
      | UMTA_assume_def assume_def ->
        [O_assume_def assume_def]

    method private by acc (by : by) =
      let cc = cc_peek acc in
      (* filter provers from by.facts. No prover means use deafault. *)
      let provers, other_bys = split_provers cc.term_db by.facts in
      let provers =
        match provers with
        | [] -> [Default]
        | x -> x
      in
      (* parse by def and add it to visible defs *)
      let additional_defs : op_def list =
        flat_map (self#parse_by_def cc.term_db) by.defs in
      let expanded_defs = append cc.expanded_defs additional_defs in
      (* parse by references to theorems *)
      let by_facts_expanded =
        expand_theorem_bodies cc.term_db cc.definitions other_bys in
      (* extend assumptions with usable facts *)
      let assumes = concat [
          by_facts_expanded;
          cc.usable_facts;
        ] in
      match cc.goal with
      | Some ccgoal ->
        (* printf "Obl %s" (format_location by.location); *)
        (*
        let goal = match ccgoal with
          | N_expr e ->
            let location = extract_location e in
            let level = max (extract_level e) (extract_level e) in
            let new_symbols = [] in
            let suffices = false in
            (* TODO: recompute boxed *)
            let boxed = false in
            { location; level; new_symbols; assumes;
              prove = e; suffices; boxed; }
        in
        *)
        let goal = ccgoal in
        let obligation = {
          id = generate_id acc;
          goal; (* = { ccgoal with assumes = append assumes ccgoal.assumes }; *)
          expanded_defs;
          location = by.location;

          constants = cc.constants;
          variables = cc.variables;
          definitions = cc.definitions;
          assumptions = cc.assumptions;
          theorems = cc.theorems;

          provers;
          term_db = cc.term_db;
        } in
        let acc0 = enqueue_obligation acc [obligation] in
        acc0
      | None ->
        enqueue_failure acc (ObligationFail ("No goal for obligation!", by.location))

    (* this method is reused for the suffices and case steps. thmi may not be
       changed for that reason. *)
    method private update_cc_formula acc (thmi:theorem_) = function
      | N_assume_prove assume_prove ->
        let cc = cc_peek acc in
        (* TODO: boxed flag might be wrong *)
        let (vs, cs, acs, ss, ts) =
          fold_left (fun (vs,cs,acs,ss,ts) sym ->
              let decl = sym.op_decl  in
              let decli = Deref.op_decl cc.term_db decl in
              match decli.kind with
              | NewVariable -> (decl::vs,cs,acs,ss,ts)
              | NewConstant -> (vs,decl::cs,acs,ss,ts)
              | NewAction   -> (vs,cs,decl::acs,ss,ts)
              | NewState    -> (vs,cs,acs,decl::ss,ts)
              | NewTemporal -> (vs,cs,acs,ss,decl::ts)
              | _ ->
                let msg = "Unexpected operator kind in ASSUME NEW construct!"
                in failwith_msg decli.location msg
            ) ([],[],[],[],[]) assume_prove.new_symbols in
        let constants = append cc.constants (rev cs) in
        (* TODO: should we extend the context by action and temporal variables? *)
        let variables = concat [cc.variables; (rev vs); (rev acs); (rev ts) ] in
        let (inner_goal, inner_thm_statements) = match thmi.definition with
          | Some td ->
            (N_expr assume_prove.prove,  (td, assume_prove.assumes) :: cc.thm_statements)
          | None ->
            (N_assume_prove assume_prove, cc.thm_statements)
        in
        let inner_cc = { cc with goal = Some inner_goal;
                                 constants;
                                 variables;
                                 thm_statements = inner_thm_statements;
                       }
        in
        let (theorems, definitions, thm_statements) = match thmi.definition with
          | None ->
            (cc.theorems, cc.definitions, cc.thm_statements)
          | Some d -> ((THM_ref thmi.id) :: cc.theorems,
                       (O_thm_def d) :: cc.definitions,
                       (d, [N_assume_prove assume_prove]) :: cc.thm_statements
                      )
        in
        (* move this to the use/hide statement
         let usable_facts =  match (thmi.name, get_nesting acc) with
           | None, InProof _ -> assume_prove :: cc.usable_facts
           (* a lemma without name does not add its statement to the context *)
           | None, Module -> cc.usable_facts
           | Some _, _ -> cc.usable_facts
           in *)
        let outer_cc = { cc with theorems;
                                 definitions;
                                 thm_statements;
                       }
        in
        OuterInner (outer_cc, inner_cc)
      | N_expr e ->
        let cc = cc_peek acc in
        let location = extract_location e in
        let level = extract_level e in
        (* TODO: check boxed flag in ap *)
        let ap = {location; level; new_symbols = []; assumes = [];
                  prove = e; suffices = false; boxed = true; } in
        let (inner_goal, inner_thm_statements) = match thmi.definition with
          | Some td ->
            (N_expr ap.prove,  (td, ap.assumes) :: cc.thm_statements)
          | None ->
            (N_assume_prove ap, cc.thm_statements)
        in
        let inner_cc = { cc with goal = Some inner_goal;
                       }
        in
        let (theorems, definitions, thm_statements) = match thmi.definition with
          | None -> (cc.theorems, cc.definitions, cc.thm_statements)
          | Some d -> ((THM_ref thmi.id) :: cc.theorems,
                       (O_thm_def d) :: cc.definitions,
                       (d, [N_assume_prove ap]) :: cc.thm_statements
                      )
        in
        let outer_cc = { cc with theorems;
                                 definitions;
                                 thm_statements;
                       } in
        OuterInner (outer_cc, inner_cc)
      | N_ap_subst_in n ->
        failwith "ap subst in node is not yet handled"

    method private update_cc_case acc (thmi:theorem_) formula =
        let cc = cc_peek acc in
        (* TODO: boxed flag might be wrong *)
        let toprove = match cc.goal with
          | None -> failwith_msg thmi.location
                      "Encountered case statement without active goal!"
          | Some f -> f
        in
        let location = extract_location formula in
        let flevel = extract_level formula in
        let level = match node_level toprove, flevel with
          (* TODO: check if the level recomputation is correct *)
          | Some l, Some f when l > f   -> Some l
          | Some l, Some f (* l <= f *) -> Some f
          | Some l, None   (* only l *) -> None
          | None,   Some f (* only f *) -> None
          | None,   None -> None
        in
        let suffices = false in
        let boxed = false in (* TODO: need to recompute boxed flag *)
        match toprove with
        | N_expr prove ->
          let new_symbols = [] in
          let assumes = [N_expr formula] in
          let ap = { location; level; new_symbols; assumes; suffices; boxed; prove; } in
          self#update_cc_formula acc thmi (N_assume_prove ap)
        | N_assume_prove tap ->
          let assumes = append tap.assumes [N_expr formula] in
          let ap = { tap with location; level; assumes; suffices; boxed; } in
          self#update_cc_formula acc thmi (N_assume_prove ap)
        | N_ap_subst_in _ ->
          failwith "unhandled case node, where node is an ap subst"

    (* \sigma pick vars : F(vars) is translated into two steps:
       \rho \E vars : F(vars)
       \sigma SUFFICES ASSUME NEW vars, F(vars)
            PROVE G

       We end up with 3 obligations:
       1: \E vars : F(vars)
       2: ASSUME \E vars : F(vars),
               ASSUME NEW c, F(c) PROVE G
        PROVE G
       3: vars, F(vars) added to assumptions of G

       where 2 is trivial.
    *)
    method private update_cc_pick acc (thmi : theorem_) {variables; formula} =
      let cc = cc_peek acc in
    (*
       let fmt = Expr_formatter.expr_formatter in
       let state = (Format.str_formatter, cc.term_db, false,
                    Expr_formatter.Expression, 0) in
       let str = fmt#expr state formula in
       Printf.printf "Pick formula starts with: %s\n"
                     (Format.flush_str_formatter ());
     *)
      let location = extract_location formula in
      let level = extract_level formula in
      let (bounded_qs, unbounded_qs) as qs =
        partition (function
            | B_bounded_bound_symbol _ -> true;
            | _ -> false ) variables in
      let get_bounds = function
        | B_bounded_bound_symbol x -> x;
        | B_unbounded_bound_symbol _ ->
          let msg =
            "Implementation error: may not encounter unbounded symbols here!"
          in
          failwith_msg thmi.location msg
      in
      let tdb = (get_cc acc |> hd).term_db in
      let (bounds, quantifier) =
        match qs with
        | ([], uqs) -> ([], Builtin.get tdb Builtin.EXISTS)
        | (bqs, []) ->
          (map get_bounds bqs, Builtin.get tdb Builtin.BEXISTS)
        | _ -> failwith_msg thmi.location
                 "Pick mixes bounded and unbounded quantifiers!"
      in
      let ex_formula =
        E_binder { location; level;
                   operator = FMOTA_op_def (O_builtin_op quantifier);
                   operand  = EO_expr formula;
                   bound_symbols = variables;
                 } in
      let bounds_formulas =
        map (function
            |  { params;
                 tuple;
                 domain;
               } ->
              ()
          )
          bounds in
      let inner_cc =
        {cc with
         goal = Some (N_expr ex_formula);
        } in
      let thm_statements = match thmi.definition with
        | Some td -> (td, [N_expr formula]) :: cc.thm_statements
        | None -> cc.thm_statements;
      in
      let outer_cc =
        {cc with
         goal = Some (N_expr formula);
         (* check this *)
         thm_statements;
        } in
      OuterInner (outer_cc, inner_cc)

    method private qed acc (thmi : theorem_) =
      let cc = cc_peek acc in
      Inner cc

    method theorem acc thm =
      let cc = cc_peek acc in
      let thmi  = Deref.theorem cc.term_db thm in
      (* we prepare one context for proving the statement and one for continuing *)
      let step_ccs = match thmi.statement with
        | ST_FORMULA f ->
          self#update_cc_formula acc thmi f
        | ST_SUFFICES f ->
          self#update_cc_formula acc thmi f
        | ST_CASE f ->
          self#update_cc_case acc thmi f
        | ST_PICK f ->
          self#update_cc_pick acc thmi f
        | ST_HAVE e ->
          NoContext (* TODO *)
        | ST_TAKE f ->
          NoContext (* TODO *)
        | ST_WITNESS f ->
          NoContext (* TODO *)
        | ST_QED ->
          self#qed acc thmi (* TODO *)
      in
      let racc = match step_ccs with
        | NoContext ->
          acc
        | Outer outer_cc ->
          (* extract outer proof *)
          cc_replace  outer_cc acc
        | Inner inner_cc ->
          (* extract inner proof *)
          let acc1 = cc_push inner_cc acc in
          let acc2 = increase_nesting acc1 in
          let acc3 = self#proof acc2 thmi.proof in
          (* we need to reset the current context, usable facts etc are not
             visible outside the sub-proof *)
          (* let inner_obs = get_obligations acc2 in
             (* we add the inner obligations to the outer context, but don't
             take over anything else *)
             update_obligations acc0 inner_obs *)
          let (_, acc4) = cc_pop acc3 in
          decrease_nesting acc4
        | OuterInner (outer_cc, inner_cc) ->
          (* extract inner proof *)
          let acc1 = cc_push inner_cc acc in
          let acc2 = increase_nesting acc1 in
          let acc3 = self#proof acc2 thmi.proof in
          (* we need to reset the current context, usable facts etc are not
             visible outside the sub-proof *)
          (* let inner_obs = get_obligations acc2 in
             let acc3 = update_obligations acc2 inner_obs in
          *)
          let (_, acc4) = cc_pop acc3 in
          (* extract outer proof *)
          cc_replace outer_cc acc4 |> decrease_nesting 
      in
      (* assert that no obligations were lost*)
      let no_oldobs = length (get_obligations acc) in
      let no_newobs = length (get_obligations racc) in
      if (no_newobs < no_oldobs) then failwith "lost obligations!";
      (*    Printf.printf "no of obs: %d delta %d\n" no_newobs (no_newobs - no_oldobs);
            Printf.printf "no of usable facts in acc %d and in racc %d\n"
                        (length (cc_peek acc).usable_facts )
                        (length (cc_peek racc).usable_facts );
      *)
      racc


    method proof acc0 = function
      | P_omitted _ -> acc0
      | P_obvious { location = {filename; _ } as location; _ }
        when filename = get_root_module acc0 ->
        (
          let cc = cc_peek acc0 in
          let goal = match cc.goal with
            | Some g -> g
            | None ->
              failwith_msg
                location
                "No goal in context while processing OBVIOUS statement!"
          in
          let obligation = {
            id = generate_id acc0;
            goal;
            location;
            expanded_defs = cc.expanded_defs;
            provers = [Default];
            term_db = cc.term_db;
            constants = cc.constants;
            variables = cc.variables;
            definitions = cc.definitions;
            assumptions = cc.assumptions;
            theorems = cc.theorems;
          } in
          enqueue_obligation acc0 [obligation]
        )
      | P_obvious _ ->
        acc0
      | P_by by ->
        self#by acc0 by
      | P_steps {steps; location; _ } ->
        fold_left self#step acc0 steps
      | P_noproof -> acc0


    method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
      let cc = cc_peek acc0 in
      let expr_facts = List.fold_left (fun acc -> function
          | EMM_expr e -> (N_expr e)::acc
          | EMM_module_instance mi ->
            failwith_msg location "don't know how to handle USE module instance!"
          | EMM_module m ->
            failwith_msg location "don't know how to handle USE module!"
        ) [] facts |> List.rev
      in
      let opdef_defs = List.fold_left (fun acc -> function
          | UMTA_assume_def a -> (O_assume_def a) :: acc
          | UMTA_theorem_def a -> (O_thm_def a) :: acc
          | UMTA_user_defined_op a -> (O_user_defined_op a) :: acc
          | UMTA_module_instance a -> (O_module_instance a) :: acc
        ) [] defs |> List.rev
      in
      let usable_facts = match hide,only with
        | false,true  -> expr_facts
        | false,false -> List.append cc.usable_facts expr_facts
        | true, false ->
          List.filter (fun x -> List.mem x expr_facts) cc.usable_facts
        | true, true -> failwith_msg location "HIDE ONLY does not make sense."
      in
      let expanded_defs = match hide,only with
        | false,true  -> opdef_defs
        | false,false -> List.append cc.expanded_defs opdef_defs
        | true, false -> filter (fun x -> List.mem x opdef_defs) cc.expanded_defs
        | true, true -> failwith_msg location "HIDE ONLY does not make sense."
      in
      let new_cc = { cc with usable_facts; expanded_defs } in
      cc_replace new_cc acc0

    method instance acc _ =
      (* TODO *)
      acc

    method mule acc0 (mi:mule) =
      let tdb = (get_cc acc0 |> List.hd).term_db in
      let m = Deref.mule tdb mi in
      self#mule_ acc0 m

    method mule_ acc0 {name; location; module_entries } =
      let acc0a = self#name acc0 name in
      let acc1 = self#location acc0a location in
      let acc = List.fold_left self#mule_entry acc1 module_entries in
      acc

    method context acc { entries; modules; root_module; } =
      let old_cc = cc_peek acc in
      let new_cc = { old_cc with term_db = entries } in
      let acc0 = cc_replace new_cc acc in
      (*    let acc1 = fold_left self#mule acc0 modules in *)
      let tdb = (get_cc acc0 |> hd).term_db in
      let root_ms = List.filter
          (fun x -> (Deref.mule tdb x).name = root_module )
          modules
      in
      let acc1 = match root_ms with
        | [m] -> self#mule acc0 m
        | [] -> failwith "root module not found!"
        | _ -> failwith "root module not unique!"
      in
      (* Printf.printf "size of stack in the end: %d\n" (length (get_cc acc1)); *)
      match length (get_cc acc1) with
      | 1 ->
           (*
    fold_left (fun i cc ->
               Printf.printf "(%d) known theorems:\n" i;
               map (fun (t : theorem) ->
                    let thmi = Deref.theorem cc.term_db t in
                    let name = match thmi.name with
                      | None -> "(none)"
                      | Some x -> x
                    in
                    Printf.printf "thm: %s : %a\n" name
                                  (fmt_statement cc.term_db)
                                  thmi.statement ;
                   ) cc.theorems;
               i+1
              ) 1 (get_cc acc1);
            *)
        acc1
      | _ ->
        failwith_msg mkDummyLocation
          "Obligation extraction: Unbalanced pushes/pops"
  end

let extract_obligations_context ({entries; root_module; _ } as context) =
  let instance = new extract_obligations in
  let iacc = EOAcc ([emptyCurrentContext entries], [], [], Module, root_module, ())
  in
  let acc = instance#context iacc context in
  get_obligations acc
