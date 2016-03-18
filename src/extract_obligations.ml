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
    goal : assume_prove option;

    (* the facts currently known *)
    usable_facts : assume_prove list;

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
    thm_statements : (theorem_ * assume_prove list) list;
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


(* the accumulator type - containing an open parameter for derived classes *)
type 'a eoacc = EOAcc of current_context list * obligation list * nesting * 'a

(* extractors for the constituents of the accumulator *)
let get_cc (EOAcc (c, _, _, _)) = c
let get_obligations (EOAcc (_, o, _, _)) = o
let get_nesting (EOAcc (_, _, n, _)) = n

(* accumulator updates *)
let update_cc (EOAcc (_,o,n,a)) cc = EOAcc (cc,o,n,a)
let update_obligations (EOAcc (cc,_,n,a)) o = EOAcc (cc,o,n,a)
let update_nesting (EOAcc (cc,o,_,a)) n = EOAcc (cc,o,n,a)

(* convenience function adding one obligation to the accumulator *)
let enqueue_obligation acc newobs =
  let obs = get_obligations acc in
  update_obligations acc (append obs newobs)

(* convenience functions managing the nesting *)
let increase_nesting acc =
  match get_nesting acc with
  | Module -> update_nesting acc (InProof 1)
  | InProof n -> update_nesting acc (InProof (n+1))

let decrease_nesting acc =
  match get_nesting acc with
  | Module -> failwith "Cannot decrease nesting level below module!"
  | InProof n -> update_nesting acc (InProof (n-1))

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

(* that doesn't work in general anymore, because builtins are expanded and
    may not be present in the term db anymore *)
(*
let find_builtin term_db opname =
  let check_bopname = function
    | OPDef_entry (O_builtin_op bop) ->
       bop.name = opname
    | _ ->
       false
  in
  match filter (fun x -> check_bopname (snd x) ) term_db with
  | [] ->
     let msg = sprintf "Could not find built in %s in term db!" opname in
     failwith msg
  | [(_,x)] ->
     x
  | _ ->
     let msg = sprintf "Found multiple built ins of %s in term db!" opname in
     failwith msg
 *)


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

(* split facts into list of theorem_ and rest *)
let split_theorem_expr_facts term_db facts =
  let split_theorem_expr_facts_ term_db =
    fold_left (fun r fact ->
               let (thms, exprs, rest) = r in
               match fact with
               | EMM_expr ((E_op_appl appl) as e)->
                  (
                    match appl.operator, appl.operands with
                    | FMOTA_theorem thm, [] ->
                       (dereference_theorem term_db thm :: thms, exprs, rest)
                    | _ ->
                       (thms, e::exprs, rest)
                  )
               | EMM_expr e ->
                  (thms, e::exprs, rest)
               | _ ->
                  (thms, exprs, fact::rest)
              )
              ([], [], [])
  in
  (* during fold we prepended, reverse lists to preserve order *)
  let (thms, exprs, facts) = split_theorem_expr_facts_ term_db facts in
  (rev thms, rev exprs, rev facts)


(* the actual visitor subclass *)
class ['a] extract_obligations =
object(self)
  inherit ['a eoacc] visitor

  method assume acc a =
    let cc = cc_peek acc in
    let assumptions = a :: cc.assumptions in
    cc_replace { cc with assumptions } acc

  method op_decl acc opdecl =
    let cc = cc_peek acc in
    let decl_instance = dereference_op_decl cc.term_db opdecl in
    let ccnew = match decl_instance.kind with
      | ConstantDecl -> { cc with constants = opdecl :: cc.constants }
      | VariableDecl -> { cc with variables = opdecl :: cc.variables }
      | _ -> cc in
    cc_replace ccnew acc

  method op_def acc opdef =
    let cc = cc_peek acc in
    let definitions = opdef :: cc.definitions in
    cc_replace { cc with definitions } acc

  method private parse_by_def term_db = function
    | UMTA_user_defined_op uop ->
       [O_user_defined_op uop]
    | UMTA_module_instance mi ->
       let mii = dereference_module_instance term_db mi in
       failwith_msg mii.location
                    "don't know what to do with module instance in BY DEF!"
    | UMTA_theorem thm ->
       let thmi = dereference_theorem term_db thm in
       failwith_msg thmi.location
                    "don't know what to do with theorem in BY DEF!"
    | UMTA_assume assume ->
       let assi = dereference_assume term_db assume in
       failwith_msg assi.location
                    "don't know what to do with assume in BY DEF!"

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
    let thm_bys, expr_bys, other_bys =
      split_theorem_expr_facts cc.term_db other_bys in
    let extract_name ((thmi:theorem_),lst) = (thmi.name, lst) in
    let cc_thm_stmts = map extract_name cc.thm_statements in
    let thm_by_stmts =
      map (fun (x : theorem_) ->
           match mem_assoc x.name cc_thm_stmts with
           | true -> assoc x.name cc_thm_stmts
           | false -> (
             let name = match x.name with
               | Some n -> n
               | None -> "(unknown)"
             in
             let tnames = mkString (function
                                     | (Some x,_) -> x
                                     | (None, _) -> "(none)"
                                   ) cc_thm_stmts in
             let msg = sprintf "Could not find by theorem %s in %s"
                               name tnames in
             failwith_msg by.location msg
           )
          ) thm_bys
    in
    let thm_assumptions = flatten thm_by_stmts in
    (* extend assumptions with usable facts *)
    let assumes = concat [
                      thm_assumptions;
                      cc.usable_facts;
                    ] in
    match cc.goal with
    | Some ccgoal ->
       printf "Obl %s" (format_location by.location);
       let obligation = {
           goal = { ccgoal with assumes = append assumes ccgoal.assumes };
           expanded_defs;

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
       (* TODO : add error obligation *)
       failwith_msg by.location "No goal for obligation!"

  (* this method is reused for the suffices and case steps. thmi may not be
   changed for that reason. *)
  method private update_cc_formula acc (thmi:theorem_) assume_prove =
    let cc = cc_peek acc in
    (* TODO: boxed flag might be wrong *)
    let goal = {assume_prove with assumes = []; } in
    let (vs, cs, acs, ss, ts) =
      fold_left (fun (vs,cs,acs,ss,ts) sym ->
                 let decl = sym.op_decl  in
                 let decli = dereference_op_decl cc.term_db decl in
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
    let inner_stmt =  (thmi, assume_prove.assumes) in
    let inner_cc = { cc with goal = Some goal;
                             constants;
                             variables;
                             thm_statements = inner_stmt :: cc.thm_statements;
                   }
    in
    let outer_stmt = (thmi, [assume_prove]) in
    let theorems = match thmi.name with
      | None -> cc.theorems
      | _ -> (THM thmi) :: cc.theorems
    in
    let usable_facts =  match (thmi.name, get_nesting acc) with
      | None, InProof _ -> assume_prove :: cc.usable_facts
      (* a lemma without name does not add its statement to the context *)
      | None, Module -> cc.usable_facts
      | Some _, _ -> cc.usable_facts
    in
    let outer_cc = { cc with theorems;
                             usable_facts;
                             thm_statements = outer_stmt :: cc.thm_statements;
                   }
    in
    OuterInner (outer_cc, inner_cc)

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
    let level = match toprove.level, flevel with
      (* TODO: check if the level recomputation is correct *)
      | Some l, Some f when l > f   -> Some l
      | Some l, Some f (* l <= f *) -> Some f
      | Some l, None   (* only l *) -> None
      | None,   Some f (* only f *) -> None
      | None,   None -> None
    in
    let assumes = append toprove.assumes [assume_prove_from_expr false formula] in
    let suffices = false in
    let boxed = false in (* TODO: need to recompute boxed flag *)
    let ap = { toprove with location; level; assumes; suffices; boxed; } in
    self#update_cc_formula acc thmi ap

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
    let (bounds, quantifier) =
      match qs with
      | ([], uqs) -> ([], unbounded_exists)
      | (bqs, []) ->
         (map get_bounds bqs, bounded_exists)
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
    let inner_statement =
      { location;
        level;
        new_symbols = [];
        assumes = [];
        prove = ex_formula;
        suffices = false;
        boxed = true;
      } in
    let outer_statement =
      { location;
        level;
        new_symbols = [];
        assumes = [];
        prove = formula;
        suffices = true;
        boxed = true;
      } in
    let inner_cc =
      {cc with
        goal = Some inner_statement;
      } in
    let outer_cc =
      {cc with
        goal = Some outer_statement;
        (* check this *)
        thm_statements = (thmi, [outer_statement]) :: cc.thm_statements;
      } in
    OuterInner (outer_cc, inner_cc)

  method private qed acc (thmi : theorem_) =
    let cc = cc_peek acc in
    Inner cc

  method theorem acc thm =
    let cc = cc_peek acc in
    let thmi  = dereference_theorem cc.term_db thm in
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
         let acc2 = self#proof acc1 thmi.proof in
         (* we need to reset the current context, usable facts etc are not
        visible outside the sub-proof *)
         (* let inner_obs = get_obligations acc2 in
     (* we add the inner obligations to the outer context, but don't
        take over anything else *)
        update_obligations acc0 inner_obs *)
         let (_, acc3) = cc_pop acc2 in
         acc3
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
         cc_replace outer_cc acc4
    in
    (* assert that no obligations were lost*)
    let no_oldobs = length (get_obligations acc) in
    let no_newobs = length (get_obligations racc) in
    if (no_newobs < no_oldobs) then failwith "lost obligations!";
    Printf.printf "no of obs: %d delta %d\n" no_newobs (no_newobs - no_oldobs);
    Printf.printf "no of usable facts in acc %d and in racc %d\n"
                  (length (cc_peek acc).usable_facts )
                  (length (cc_peek racc).usable_facts );
    decrease_nesting racc


  method proof acc0 = function
    | P_omitted _ -> acc0
    | P_obvious {location; _} ->
       (
         let cc = cc_peek acc0 in
         let goal = match cc.goal with
           | Some g -> g
           | None ->
              failwith_msg location
                           "No goal in context while processing OBVIOUS statement!"
         in
         let obligation = {
             goal;
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
    | P_by by ->
       self#by acc0 by
    | P_steps {steps; location; _ } ->
       fold_left self#step acc0 steps
    | P_noproof -> acc0


  method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
    let cc = cc_peek acc0 in
    (* extract new facts *)
    let (thm_facts, expr_facts, rest) =
      split_theorem_expr_facts cc.term_db facts in
    let ap_facts = map (assume_prove_from_expr false) expr_facts in
    match hide with
    | false -> (* USE *)
       (* create obligations proving new facts *)
       (* TODO *)
       (* update context *)
       let new_cc = { cc with usable_facts = concat [cc.usable_facts;
                                                     ap_facts;
                                                    ]} in
       cc_replace new_cc acc0
    | true -> (* HIDE *)
       (* you cannot hide an asserted fact, i.e. expr_facts must be empty *)
       (* TODO: hide theorem/step references and defs*)
       acc0

  method instance acc _ =
    (* TODO *)
    acc

  method mule acc0 = function
    | MOD_ref i -> self#reference acc0 i
    | MOD {name; location; module_entries } ->
       let acc0a = self#name acc0 name in
       let acc1 = self#location acc0a location in
       let acc = List.fold_left self#mule_entry acc1 module_entries in
       acc


  method context acc { entries; modules;} =
    let old_cc = cc_peek acc in
    let new_cc = { old_cc with term_db = entries } in
    let acc0 = cc_replace new_cc acc in
    let acc1 = fold_left self#mule acc0 modules in
    Printf.printf "size of stack in the end: %d\n" (length (get_cc acc1));
    (*
    fold_left (fun i cc ->
               Printf.printf "(%d) known theorems:\n" i;
               map (fun (t : theorem) ->
                    let thmi = dereference_theorem cc.term_db t in
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
end
